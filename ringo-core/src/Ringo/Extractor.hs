{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ringo.Extractor
       ( extractDimensionTables
       , extractAllDimensionTables
       , extractFactTable
       , extractDependencies
       ) where

import qualified Data.Map  as Map
import qualified Data.Tree as Tree

import Prelude.Compat
import Control.Monad.Reader (Reader, asks)
import Data.Maybe           (fromJust)
import Data.Monoid          ((<>))
import Data.List            (nub)

import Ringo.Extractor.Internal
import Ringo.Types.Internal
import Ringo.Utils

extractFactTable ::  Fact -> Reader Config Table
extractFactTable fact = mkTable <$> asks configSettings
                                <*> extractColumns       fact
                                <*> extractFKColumns     fact
                                <*> extractUKColumnNames fact
  where
    mkTable Settings {..} columns fkColumns ukColNames =
      Table { tableName        =
               extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit
            , tableColumns     = columns ++ fkColumns
            , tableConstraints = [ UniqueKey $ ukColNames ++ map columnName fkColumns ]
            }

extractColumns :: Fact -> Reader Config [Column]
extractColumns fact = do
  Settings {..} <- asks configSettings
  tables        <- asks configTables
  let table     =  fromJust . findTable (factTableName fact) $ tables

  let sourceColumn cName                     = fromJust . findColumn cName . tableColumns $ table
      notNullSourceColumnCopy cName          = (sourceColumn cName) { columnNullable = NotNull }
      notNullSourceColumnRename scName cName = (notNullSourceColumnCopy scName) { columnName = cName }

  return $ concatFor (factColumns fact) $ \FactColumn {factColTargetColumn = cName, ..} ->
    case factColType of
      DimTime                ->
        [ Column (timeUnitColumnName settingDimTableIdColumnName cName settingTimeUnit) "bigint" NotNull ]
      NoDimId                -> [ notNullSourceColumnCopy cName ]
      TenantId               -> [ notNullSourceColumnCopy cName ]
      FactCount {..}         -> [ Column cName settingFactCountColumnType NotNull ]
      FactCountDistinct {..} -> [ Column cName "json" NotNull ]
      FactSum {..}           -> [ notNullSourceColumnRename factColSourceColumn cName ]
      FactMax {..}           -> [ notNullSourceColumnRename factColSourceColumn cName ]
      FactMin {..}           -> [ notNullSourceColumnRename factColSourceColumn cName ]
      FactAverage {..}       ->
        [ Column (cName <> settingAvgCountColumnSuffix) settingFactCountColumnType NotNull
        , notNullSourceColumnRename factColSourceColumn (cName <> settingAvgSumColumnSuffix)
        ]
      _                      -> []

extractFKColumns :: Fact -> Reader Config [Column]
extractFKColumns fact = do
  allDims       <- extractAllDimensionTables fact
  Settings {..} <- asks configSettings
  tables        <- asks configTables

  return $ for allDims $ \(dimFact, dimTable) ->
    let colName = factDimFKIdColumnName settingDimPrefix settingDimTableIdColumnName dimFact dimTable tables
        colType = idColTypeToFKIdColType settingDimTableIdColumnType
    in Column colName colType NotNull

extractUKColumnNames :: Fact -> Reader Config [ColumnName]
extractUKColumnNames fact = do
  Settings {..} <- asks configSettings
  return $ forMaybe (factColumns fact) $ \FactColumn {factColTargetColumn = cName, ..} ->
      case factColType of
          DimTime  -> Just $ timeUnitColumnName settingDimTableIdColumnName cName settingTimeUnit
          NoDimId  -> Just cName
          TenantId -> Just cName
          _        -> Nothing

extractDependencies :: Fact -> Reader Config Dependencies
extractDependencies fact = Map.union <$> extractFactDeps fact <*> extractDimensionDeps fact

extractFactDeps :: Fact -> Reader Config Dependencies
extractFactDeps fact = do
  Settings{..} <- asks configSettings
  facts        <- asks configFacts

  let extractedTable =
        extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit

      factSourceDeps    =
        nub . Tree.flatten . flip Tree.unfoldTree fact $ \fct ->
          (factTableName fct, parentFacts fct facts)

      factDimDeps       =
        nub . concat . Tree.flatten . flip Tree.unfoldTree fact $ \fct ->
          ( forMaybe (factColumns fct) $ \FactColumn {..} -> case factColType of
              DimVal {..} -> Just $ settingDimPrefix  <> factColTargetTable
              DimId {..}  -> Just factColTargetTable
              _           -> Nothing
          , parentFacts fct facts
          )

  return $ Map.singleton extractedTable (factSourceDeps ++ factDimDeps)
  where
    parentFacts fct facts = [ fromJust $ findFact pf facts | pf <- factParentNames fct ]

extractDimensionDeps :: Fact -> Reader Config Dependencies
extractDimensionDeps fact = do
  Settings{..} <- asks configSettings
  return $ Map.fromList [ (settingDimPrefix <> table, [factTableName fact])
                          | FactColumn {factColType = DimVal table} <- factColumns fact ]
