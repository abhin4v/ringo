{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}

module Ringo.Generator.Populate.Dimension
  ( dimensionTablePopulationSQL
  , dimensionTablePopulationStatement
  ) where

import Prelude.Compat
import Control.Monad.Reader     (Reader, asks)
import Database.HsSqlPpp.Syntax ( Statement, SelectList, QueryExpr(..), ScalarExpr
                                , Distinct(..), makeSelect, JoinType(..) )
import Data.Maybe               (fromJust)
import Data.Text                (Text)

import Ringo.Extractor.Internal
import Ringo.Generator.Internal
import Ringo.Generator.Sql
import Ringo.Types.Internal

dimensionTablePopulationSQL :: TablePopulationMode -> Fact -> TableName -> Reader Env Text
dimensionTablePopulationSQL popMode fact dimTableName =
  ppStatement <$> dimensionTablePopulationStatement popMode fact dimTableName

makeSelectList :: Fact -> [(ColumnName, ColumnName)] -> Reader Env SelectList
makeSelectList fact colMapping = do
  tables        <- asks envTables
  defaults      <- asks envTypeDefaults
  let factTable = fromJust $ findTable (factTableName fact) tables
  return $ sl [ flip sia (nmc cName) $ coalesceColumn defaults (factTableName fact) col
                | (_, cName) <- colMapping
                , let col    = fromJust . findColumn cName $ tableColumns factTable ]

makeSelectWhereClause :: TablePopulationMode -> Fact -> [(a, Text)] -> Maybe ScalarExpr
makeSelectWhereClause popMode fact colMapping = let
    timeCol    = head ([ cName | FactColumn cName DimTime <- factColumns fact ] :: [ColumnName])
    isNotNullC = parens . foldBinop "or" . map (postop "isnotnull" . ei . snd) $ colMapping
  in Just . foldBinop "and" $
       [ isNotNullC, binop "<" (ei timeCol) placeholder ] ++
         [ binop ">=" (ei timeCol) placeholder | popMode == IncrementalPopulation ]

makeIncSelectQuery :: QueryExpr -> TableName -> [(ColumnName, ColumnName)] -> QueryExpr
makeIncSelectQuery selectQ dimTableName colMapping =
  makeSelect
    { selSelectList = sl [si $ qstar alias]
    , selTref       =
        [ tjoin (subtrefa alias selectQ) LeftOuter (tref dimTableName) . Just $
            foldBinop "and" [ binop "=" (eqi dimTableName c1) (eqi alias c2) | (c1, c2) <- colMapping ] ]
    , selWhere      =
        Just . foldBinop "and" . map (postop "isnull" . eqi dimTableName . fst) $ colMapping
    }
  where
    alias = "x"

makeSelectQuery :: TablePopulationMode -> Fact -> TableName -> [(ColumnName, ColumnName)] -> Reader Env QueryExpr
makeSelectQuery popMode fact dimTableName colMapping = do
  selectList <- makeSelectList fact colMapping
  let selectQ = makeSelect
        { selDistinct   = Distinct
        , selSelectList = selectList
        , selTref       = [tref $ factTableName fact]
        , selWhere      = makeSelectWhereClause popMode fact colMapping
        }

  return $ case popMode of
    FullPopulation        -> selectQ
    IncrementalPopulation -> makeIncSelectQuery selectQ dimTableName colMapping

dimensionTablePopulationStatement :: TablePopulationMode -> Fact -> TableName -> Reader Env Statement
dimensionTablePopulationStatement popMode fact dimTableName = do
  Settings {..}  <- asks envSettings
  let colMapping = dimColumnMapping settingDimPrefix fact dimTableName
  let iTableName = suffixTableName popMode settingTableNameSuffixTemplate dimTableName
  selectQ        <- makeSelectQuery popMode fact dimTableName colMapping
  return $ insert iTableName (map fst colMapping) selectQ
