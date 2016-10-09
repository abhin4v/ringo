{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module Ringo.Generator.Populate.Fact
  ( factTablePopulationSQL
  , factTablePopulationStatements
  , ilog2FunctionString
  ) where

import qualified Data.Text as Text

import Prelude.Compat
import Control.Monad.Reader     (Reader, asks)
import Database.HsSqlPpp.Syntax ( QueryExpr(..), ScalarExpr, Statement, makeSelect, NameComponent
                                , JoinType(..) )
import Data.List                (nub)
import Data.Maybe               (fromJust, listToMaybe)
import Data.Monoid              ((<>))
import Data.Text                (Text)
import Text.RawString.QQ        (r)

import Ringo.Extractor.Internal
import Ringo.Generator.Internal
import Ringo.Generator.Sql
import Ringo.Generator.Populate.Fact.CountDistinct
import Ringo.Types.Internal
import Ringo.Utils

ilog2FunctionString :: Text
ilog2FunctionString = [r|CREATE OR REPLACE FUNCTION ilog2(v integer)
    RETURNS integer AS
$$
DECLARE
    r integer;
    shift integer;
BEGIN
    IF v > x'FFFF'::integer THEN r := 1 << 4; ELSE r := 0 << 4; END IF;
    v := v >> r;
    IF v > x'FF'::integer THEN shift := 1 << 3; ELSE shift := 0 << 3; END IF;
    v := v >> shift;
    r := r | shift;
    IF v > x'F'::integer THEN shift := 1 << 2; ELSE shift := 0 << 2; END IF;
    v := v >> shift;
    r := r | shift;
    IF v > x'3'::integer THEN shift := 1 << 1; ELSE shift := 0 << 3; END IF;
    v := v >> shift;
    r := r | shift;
    r := r | (v >> 1);
    RETURN r;
END;
$$
LANGUAGE 'plpgsql' IMMUTABLE;
|]

factTablePopulationSQL :: TablePopulationMode -> Fact -> Reader Config [Text]
factTablePopulationSQL popMode fact = do
  stmts <- factTablePopulationStatements popMode fact
  return $ case stmts of
    []   -> []
    [i]  -> [ ppStatement i ]
    i:us -> [ ppStatement i, ilog2FunctionString ] ++ map ppStatement us

factTablePopulationStatements :: TablePopulationMode -> Fact -> Reader Config [Statement]
factTablePopulationStatements popMode fact = do
  Settings {..}        <- asks configSettings
  allDims              <- extractAllDimensionTables fact
  selExprs             <- selectExprs popMode fact allDims groupByColPrefix
  popQueryExpr         <- populateQueryExpr popMode fact allDims selExprs groupByColPrefix

  let extFactTableName = suffixTableName popMode settingTableNameSuffixTemplate
        $ extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit
      insertIntoStmt   = insert extFactTableName (map fst3 selExprs) popQueryExpr

  updateStmts          <- factCountDistinctUpdateStatements popMode fact groupByColPrefix popQueryExpr
  return $ insertIntoStmt : updateStmts
  where
    groupByColPrefix = "xxff_"

selectExprs :: TablePopulationMode
            -> Fact
            -> [(Fact, Table)]
            -> Text
            -> Reader Config [(ColumnName, (ScalarExpr, NameComponent), Bool)]
selectExprs popMode fact allDims groupByColPrefix = do
  factSelExprs <- factColumnSelectExprs fact
  dimSelExprs  <- dimColumnSelectExprs popMode allDims

  return [ (cName, (expr, nmc $ groupByColPrefix <> cName), addToGroupBy)
           | (cName, expr, addToGroupBy) <- factSelExprs ++ dimSelExprs ]

factColumnSelectExprs :: Fact -> Reader Config [(ColumnName, ScalarExpr, Bool)]
factColumnSelectExprs fact = do
  Settings {..}    <- asks configSettings
  tables           <- asks configTables
  typeDefaults     <- asks configTypeDefaults
  let fTableName   = factTableName fact
      fTable       = fromJust . findTable fTableName $ tables
      dimIdColName = settingDimTableIdColumnName
      app' f cName = app f [ eqi fTableName cName ]

  return $ concatFor (factColumns fact) $ \FactColumn {factColTargetColumn = cName, ..} ->
    case factColType of
      DimTime                -> [ timeUnitColumnSelectExpr fTableName dimIdColName settingTimeUnit cName ]
      NoDimId                -> [ dimIdColumnSelectExpr fTableName fTable typeDefaults cName ]
      TenantId               -> [ dimIdColumnSelectExpr fTableName fTable typeDefaults cName ]
      FactCount {..}         ->
        [ (cName, app "count" [ maybe star (eqi fTableName) factColMaybeSourceColumn ], False) ]
      FactCountDistinct {..} -> [ (cName, cast (str "{}") "json", False) ]
      FactSum {..}           -> [ (cName, app' "sum" factColSourceColumn, False) ]
      FactMax {..}           -> [ (cName, app' "max" factColSourceColumn, False) ]
      FactMin {..}           -> [ (cName, app' "min" factColSourceColumn, False) ]
      FactAverage {..}       ->
        [ ( cName <> settingAvgCountColumnSuffix, app' "count" factColSourceColumn, False )
        , ( cName <> settingAvgSumColumnSuffix , app' "sum" factColSourceColumn  , False)
        ]
      _                      -> []

timeUnitColumnSelectExpr :: TableName -> ColumnName -> TimeUnit -> ColumnName -> (ColumnName, ScalarExpr, Bool)
timeUnitColumnSelectExpr fTableName dimIdColName settingTimeUnit cName =
  let colName = timeUnitColumnName dimIdColName cName settingTimeUnit
  in ( colName
     , cast (app "floor" [ binop "/" (extEpoch (eqi fTableName cName))
                                     (num . Text.pack . show . timeUnitToSeconds $ settingTimeUnit) ])
            "bigint"
     , True
     )

dimIdColumnSelectExpr :: TableName -> Table -> TypeDefaults -> ColumnName -> (ColumnName, ScalarExpr, Bool)
dimIdColumnSelectExpr fTableName fTable typeDefaults cName =
  let sCol = fromJust . findColumn cName $ tableColumns fTable
  in (cName, coalesceColumn typeDefaults fTableName sCol, True)

dimColumnSelectExprs :: TablePopulationMode -> [(Fact, Table)] -> Reader Config [(ColumnName, ScalarExpr, Bool)]
dimColumnSelectExprs popMode allDims = do
  settings@Settings {..} <- asks configSettings
  tables                 <- asks configTables
  typeDefaults           <- asks configTypeDefaults
  let dimIdColName       = settingDimTableIdColumnName

  return $ for allDims $ \(dimFact, factTable@Table {tableName}) -> let
      dimFKIdColName        =
        factDimFKIdColumnName settingDimPrefix dimIdColName dimFact factTable tables
      factSourceTableName   = factTableName dimFact
      factSourceTable       = fromJust . findTable factSourceTableName $ tables
      dimFKIdColumn         = fromJust . findColumn dimFKIdColName $ tableColumns factSourceTable
      dimLookupWhereClauses = Just . foldBinop "and" $
        [ binop "=" (eqi tableName dimColName) (coalesceColumn typeDefaults factSourceTableName sourceCol)
          | (dimColName, sourceColName) <- dimColumnMapping settingDimPrefix dimFact tableName
          , let sourceCol = fromJust . findColumn sourceColName $ tableColumns factSourceTable ]
      insertExpr = if factTable `elem` tables -- existing dimension table
        then (if columnNullable dimFKIdColumn == Null then coalesceFKId settings else id)
               $ eqi factSourceTableName dimFKIdColName
        else coalesceFKId settings . subQueryExp $
               makeSelect
                 { selSelectList = sl [ si $ ei dimIdColName ]
                 , selTref       =
                     [ trefa (suffixTableName popMode settingTableNameSuffixTemplate tableName) tableName ]
                 , selWhere      = dimLookupWhereClauses
                 }
     in (dimFKIdColName, insertExpr, True)
  where
    coalesceFKId Settings {..} ex =
      app "coalesce" [ ex, num . Text.pack . show $ settingForeignKeyIdCoalesceValue ]

populateQueryExpr :: TablePopulationMode
                  -> Fact
                  -> [(Fact, Table)]
                  -> [(ColumnName, (ScalarExpr, NameComponent), Bool)]
                  -> Text
                  -> Reader Config QueryExpr
populateQueryExpr popMode fact allDims selExprs groupByColPrefix = do
  Settings {..}   <- asks configSettings
  tables          <- asks configTables
  let fTableName  = factTableName fact
      fTable      = fromJust . findTable fTableName $ tables
      joinClauses =
        map (tref &&& joinClausePreds fTable)
        . filter (/= fTableName)
        . nub
        . map (factTableName . fst)
        $ allDims
      timeCol     = eqi fTableName $ head [ cName | FactColumn cName DimTime <- factColumns fact ]
  return $ makeSelect
             { selSelectList = sl . map (uncurry sia . snd3) $ selExprs
             , selTref       = [ foldl (\tf (t, oc) -> tjoin tf LeftOuter t oc) (tref fTableName) joinClauses ]
             , selWhere      = Just . foldBinop "and" $
                 binop "<" timeCol placeholder :
                   [ binop ">=" timeCol placeholder | popMode == IncrementalPopulation ]
             , selGroupBy    = map (ei . (groupByColPrefix <>) . fst3) . filter thd3 $ selExprs
             }
  where
    joinClausePreds table oTableName =
      foldBinop "and"
      . map (\(c1, c2) -> binop "=" (eqi (tableName table) c1) (eqi oTableName c2))
      <$> listToMaybe [ colPairs | ForeignKey tName colPairs <-  tableConstraints table
                                 , tName == oTableName ]
