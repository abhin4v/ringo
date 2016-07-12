{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ringo.Extractor.Internal where

import qualified Data.Map  as Map
import qualified Data.Text as Text

import Prelude.Compat
import Control.Monad.Reader (Reader, asks)
import Data.Function        (on)
import Data.Maybe           (mapMaybe, fromMaybe, fromJust, catMaybes)
import Data.Monoid          ((<>))
import Data.List            (nub, nubBy, find)
import Data.Text            (Text)

import Ringo.Types.Internal
import Ringo.Utils

findTable :: TableName -> [Table] -> Maybe Table
findTable tName = find ((== tName) . tableName)

findFact :: TableName -> [Fact] -> Maybe Fact
findFact fName = find ((== fName) . factName)

findColumn :: ColumnName -> [Column] -> Maybe Column
findColumn cName = find ((== cName) . columnName)

dimColumnName :: Text -> ColumnName -> ColumnName
dimColumnName dimName columnName =
  fromMaybe columnName . Text.stripPrefix (dimName <> "_") $ columnName

dimColumnMapping :: Text -> Fact -> TableName -> [(ColumnName, ColumnName)]
dimColumnMapping dimPrefix fact dimTableName =
  [ (dimColumnName factColTargetTable factColTargetColumn, factColTargetColumn)
    | FactColumn { factColType = DimVal {..}, ..} <- factColumns fact
    , dimPrefix <> factColTargetTable == dimTableName ]

dimColumnMappings :: Text -> Fact -> [(TableName, [(ColumnName, ColumnName)])]
dimColumnMappings dimPrefix fact =
  nub [ (dimTableName, dimColumnMapping dimPrefix fact dimTableName)
        | FactColumn { factColType = DimVal {..}, ..} <- factColumns fact
        , let dimTableName = dimPrefix <> factColTargetTable ]

timeUnitColumnName :: Text -> ColumnName -> TimeUnit -> ColumnName
timeUnitColumnName dimIdColName colName timeUnit =
  colName <> "_" <> timeUnitName timeUnit <> "_" <> dimIdColName

factDimFKIdColumnName :: Text -> Text -> Fact -> Table -> [Table] -> ColumnName
factDimFKIdColumnName dimPrefix dimIdColName dimFact dimTable@Table { .. } tables =
  if dimTable `elem` tables
    then head [ factColTargetColumn
                | FactColumn {factColType = DimId {..}, ..} <- factColumns dimFact
                , factColTargetTable == tableName ]
    else fromMaybe tableName (Text.stripPrefix dimPrefix tableName) <> "_" <> dimIdColName

extractedFactTableName :: Text -> Text -> TableName -> TimeUnit -> TableName
extractedFactTableName factPrefix factInfix factName timeUnit =
  factPrefix <> factName <> factInfix <> timeUnitName timeUnit

idColTypeToFKIdColType :: Text -> Text
idColTypeToFKIdColType typ = case Text.toLower typ of
  "serial"      -> "integer"
  "smallserial" -> "smallint"
  "bigserial"   -> "bigint"
  _             -> typ

extractDimensionTables :: Fact -> Reader Config [Table]
extractDimensionTables fact = do
  settings  <- asks configSettings
  tables    <- asks configTables
  return $ dimTablesFromIds tables fact ++ dimTablesFromVals settings tables fact

dimTablesFromIds :: [Table] -> Fact -> [Table]
dimTablesFromIds tables fact =
  catMaybes [ findTable factColTargetTable tables
              | FactColumn { factColType = DimId {..} } <- factColumns fact ]

dimTablesFromVals :: Settings -> [Table] -> Fact -> [Table]
dimTablesFromVals Settings {..} tables fact =
  fact
  >>- factColumns
  >>> mapMaybe (findDimValColumn . Just)
  >>> Map.fromListWith (flip (++))
  >>> Map.mapWithKey makeDimColumns
  >>> Map.toList
  >>> map (uncurry makeDimTable)
  where
    Table {..} = fromJust . findTable (factTableName fact) $ tables

    makeDimTable dim cols =
      Table { tableName        = settingDimPrefix <> dim
            , tableColumns     =
                Column settingDimTableIdColumnName settingDimTableIdColumnType NotNull : cols
            , tableConstraints = [ PrimaryKey settingDimTableIdColumnName
                                 , UniqueKey (map columnName cols)
                                 ]
            }

    makeDimColumns dim cols = [ col { columnName = dimColumnName dim (columnName col)
                                    , columnNullable = NotNull
                                    }
                                | col <- nub cols
                              ]

    findDimValColumn :: Maybe FactColumn -> Maybe (TableName, [Column])
    findDimValColumn fcol = do
      FactColumn { factColType = DimVal {..}, .. } <- fcol
      column <- findColumn factColTargetColumn tableColumns
      return (factColTargetTable, [column])

extractAllDimensionTables :: Fact -> Reader Config [(Fact, Table)]
extractAllDimensionTables fact = do
  myDims     <- map (fact,) <$> extractDimensionTables fact
  parentDims <- concat <$> mapM extract (factParentNames fact)
  return . nubBy ((==) `on` snd) $ myDims ++ parentDims
  where
    extract fName = asks configFacts >>= extractAllDimensionTables . fromJust . findFact fName
