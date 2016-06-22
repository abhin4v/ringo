{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ringo.Types.Internal where

import qualified Data.Text as Text

import Data.Map    (Map)
import Data.Monoid ((<>))
import Data.Text   (Text)

showColNames :: [Text] -> String
showColNames cols = Text.unpack $ "(" <> Text.intercalate ", " cols <> ")"

type ColumnName = Text
type ColumnType = Text
type TableName  = Text

data Nullable = Null | NotNull deriving (Eq, Enum)

instance Show Nullable where
  show Null    = "NULL"
  show NotNull = "NOT NULL"

data Column = Column
              { columnName     :: !ColumnName
              , columnType     :: !ColumnType
              , columnNullable :: !Nullable
              } deriving (Eq)

instance Show Column where
  show Column {..} = "Column "
                       ++ Text.unpack columnName ++ " "
                       ++ Text.unpack columnType ++ " "
                       ++ show columnNullable

data TableConstraint = PrimaryKey !ColumnName
                     | UniqueKey  ![ColumnName]
                     | ForeignKey !TableName ![(ColumnName, ColumnName)]
                     deriving (Eq)

instance Show TableConstraint where
  show (PrimaryKey col)          = "PrimaryKey " ++ Text.unpack col
  show (UniqueKey cols)          = "UniqueKey " ++ showColNames cols
  show (ForeignKey tName colMap) = "ForeignKey " ++ showColNames (map fst colMap) ++ " "
                                     ++ Text.unpack tName ++  " " ++ showColNames (map snd colMap)
data Table = Table
             { tableName        :: !TableName
             , tableColumns     :: ![Column]
             , tableConstraints :: ![TableConstraint]
             } deriving (Eq)

instance Show Table where
  show Table {..} =
    unlines $ ("Table " ++ Text.unpack tableName) : map show tableColumns ++ map show tableConstraints

data TimeUnit = Second | Minute | Hour | Day | Week
                deriving (Eq, Enum, Show, Read)

timeUnitName :: TimeUnit -> Text
timeUnitName = Text.toLower . Text.pack . show

timeUnitToSeconds :: TimeUnit -> Int
timeUnitToSeconds Second = 1
timeUnitToSeconds Minute = 60 * timeUnitToSeconds Second
timeUnitToSeconds Hour   = 60 * timeUnitToSeconds Minute
timeUnitToSeconds Day    = 24 * timeUnitToSeconds Hour
timeUnitToSeconds Week   = 7  * timeUnitToSeconds Day

data Fact = Fact
            { factName            :: !TableName
            , factTableName       :: !TableName
            , factTablePersistent :: !Bool
            , factParentNames     :: ![TableName]
            , factColumns         :: ![FactColumn]
            } deriving (Show)

data FCTNone
data FCTTargetTable
data FCTMaybeSourceColumn
data FCTSourceColumn

data FactColumnType a where
  DimTime           ::                                                        FactColumnType FCTNone
  NoDimId           ::                                                        FactColumnType FCTNone
  TenantId          ::                                                        FactColumnType FCTNone
  DimId             :: { factColTargetTable  :: !TableName }               -> FactColumnType FCTTargetTable
  DimVal            :: { factColTargetTable  :: !TableName }               -> FactColumnType FCTTargetTable
  FactCount         :: { factColMaybeSourceColumn :: !(Maybe ColumnName) } -> FactColumnType FCTMaybeSourceColumn
  FactCountDistinct :: { factColMaybeSourceColumn :: !(Maybe ColumnName) } -> FactColumnType FCTMaybeSourceColumn
  FactSum           :: { factColSourceColumn  :: !ColumnName }             -> FactColumnType FCTSourceColumn
  FactAverage       :: { factColSourceColumn  :: !ColumnName }             -> FactColumnType FCTSourceColumn
  FactMax           :: { factColSourceColumn  :: !ColumnName }             -> FactColumnType FCTSourceColumn
  FactMin           :: { factColSourceColumn  :: !ColumnName }             -> FactColumnType FCTSourceColumn

deriving instance Show (FactColumnType a)

data FactColumn = forall a. FactColumn
                  { factColTargetColumn :: !ColumnName
                  , factColType         :: FactColumnType a }

deriving instance Show FactColumn

factSourceColumnName :: FactColumn -> Maybe ColumnName
factSourceColumnName FactColumn {..} = case factColType of
  DimTime                -> Just factColTargetColumn
  NoDimId                -> Just factColTargetColumn
  TenantId               -> Just factColTargetColumn
  DimId {..}             -> Just factColTargetColumn
  DimVal {..}            -> Just factColTargetColumn
  FactCount {..}         -> factColMaybeSourceColumn
  FactCountDistinct {..} -> factColMaybeSourceColumn
  FactSum {..}           -> Just factColSourceColumn
  FactAverage {..}       -> Just factColSourceColumn
  FactMax {..}           -> Just factColSourceColumn
  FactMin {..}           -> Just factColSourceColumn

data Settings = Settings
                { settingDimPrefix                  :: !Text
                , settingFactPrefix                 :: !Text
                , settingTimeUnit                   :: !TimeUnit
                , settingAvgCountColumSuffix        :: !Text
                , settingAvgSumColumnSuffix         :: !Text
                , settingDimTableIdColumnName       :: !Text
                , settingDimTableIdColumnType       :: !Text
                , settingFactCountColumnType        :: !Text
                , settingFactCountDistinctErrorRate :: !Double
                , settingFactInfix                  :: !Text
                , settingDependenciesJSONFileName   :: !Text
                , settingFactsJSONFileName          :: !Text
                , settingDimensionJSONFileName      :: !Text
                , settingForeignKeyIdCoalesceValue  :: !Int
                , settingTableNameSuffixTemplate    :: !Text
                } deriving (Eq, Show)

defSettings :: Settings
defSettings = Settings
              { settingDimPrefix                  = "dim_"
              , settingFactPrefix                 = "fact_"
              , settingTimeUnit                   = Minute
              , settingAvgCountColumSuffix        = "_count"
              , settingAvgSumColumnSuffix         = "_sum"
              , settingDimTableIdColumnName       = "id"
              , settingDimTableIdColumnType       = "serial"
              , settingFactCountColumnType        = "integer"
              , settingFactCountDistinctErrorRate = 0.05
              , settingFactInfix                  = "_by_"
              , settingDependenciesJSONFileName   = "dependencies.json"
              , settingFactsJSONFileName          = "facts.json"
              , settingDimensionJSONFileName      = "dimensions.json"
              , settingForeignKeyIdCoalesceValue  = -1
              , settingTableNameSuffixTemplate    = "{{suff}}"
              }

data ValidationError = MissingTable             !TableName
                     | DuplicateTable           !TableName
                     | MissingFact              !TableName
                     | DuplicateFact            !TableName
                     | MissingColumn            !TableName !ColumnName
                     | DuplicateColumn          !TableName !ColumnName
                     | MissingTimeColumn        !TableName
                     | MissingNotNullConstraint !TableName !ColumnName
                     | MissingTypeDefault       !Text
                     deriving (Eq, Show)

type TypeDefaults = Map Text Text

data Env = Env
           { _envTables       :: ![Table]
           , _envFacts        :: ![Fact]
           , _envSettings     :: !Settings
           , _envTypeDefaults :: !TypeDefaults
           } deriving (Show)

envTables       :: Env -> [Table]
envTables       = _envTables

envFacts        :: Env -> [Fact]
envFacts        = _envFacts

envSettings     :: Env -> Settings
envSettings     = _envSettings

envTypeDefaults :: Env -> TypeDefaults
envTypeDefaults = _envTypeDefaults

data TablePopulationMode = FullPopulation | IncrementalPopulation deriving (Eq, Show)

type Dependencies = Map TableName [TableName]
