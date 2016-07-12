{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ringo.Types.Internal where

import qualified Data.Text as Text

import Data.Map    (Map)
import Data.Monoid ((<>))
import Data.Text   (Text)

showColNames :: [Text] -> String
showColNames cols = Text.unpack $ "(" <> Text.intercalate ", " cols <> ")"

-- | Name of a 'Column'
type ColumnName = Text

-- | Type of a 'Column'
type ColumnType = Text

-- | Name of a 'Table'
type TableName  = Text

-- | Nullness of a 'Column'
data Nullable =
    Null    -- ^ the column is nullable
  | NotNull -- ^ the column is not nullable
  deriving (Eq, Enum)

instance Show Nullable where
  show Null    = "NULL"
  show NotNull = "NOT NULL"

-- | A column of a 'Table'
data Column = Column
              { columnName     :: !ColumnName -- ^ Name of the column
              , columnType     :: !ColumnType -- ^ Type of the column
              , columnNullable :: !Nullable   -- ^ Nullness of the column
              } deriving (Eq)

instance Show Column where
  show Column {..} = "Column "
                       ++ Text.unpack columnName ++ " "
                       ++ Text.unpack columnType ++ " "
                       ++ show columnNullable

-- | A constraint on a 'Table'
data TableConstraint =
 -- | A primary key constraint
   PrimaryKey
   { tableConstrPrimaryKeyColumn :: !ColumnName -- ^ Name of the primary key column
   }
 -- | A unique key contraint
 | UniqueKey
   { tableConstrUniqueKeyColumns :: ![ColumnName] -- ^ Name of the unique key columns
   }
 -- | A foreign key constraint
 | ForeignKey
   { -- | Name of the table referenced by the foreign key
     tableConstrForeignKeyTable         :: !TableName
     -- | Mapping of the columns as an associative list for the foreign key.
     --   keys: this table's column names, values: referenced table's column names.
   , tableConstrForeignKeyColumnMapping :: ![(ColumnName, ColumnName)]
   }
 deriving (Eq)

instance Show TableConstraint where
  show (PrimaryKey col)          = "PrimaryKey " ++ Text.unpack col
  show (UniqueKey cols)          = "UniqueKey " ++ showColNames cols
  show (ForeignKey tName colMap) = "ForeignKey " ++ showColNames (map fst colMap) ++ " "
                                     ++ Text.unpack tName ++  " " ++ showColNames (map snd colMap)

-- | A table representing a physical table in the database
--
-- The following example represents a set of tables from a multi-publisher blog system:
--
-- >>> :set -XOverloadedStrings
-- >>> :{
-- let publishersTable =
--       Table { tableName = "publishers"
--             , tableColumns =
--               [ Column "id" "integer"        NotNull
--               , Column "name" "varchar(100)" NotNull
--               ]
--             , tableConstraints =
--               [ PrimaryKey "id"
--               , UniqueKey [ "name" ]
--               ]
--             }
--     usersTable =
--       Table { tableName = "users"
--             , tableColumns =
--               [ Column "id" "uuid"               NotNull
--               , Column "created_at" "timestamp"  NotNull
--               , Column "pub_id" "integer"        NotNull
--               , Column "username" "varchar(100)" NotNull
--               , Column "email" "varchar(500)"    Null
--               ]
--             , tableConstraints =
--               [ PrimaryKey "id"
--               , ForeignKey "publishers" [ ("pub_id", "id") ]
--               , UniqueKey [ "pub_id", "username" ]
--               ]
--             }
--     -- This table records the time spent by each user on a post
--     postViewEventsTable =
--       Table { tableName = "post_view_events"
--             , tableColumns =
--               [ Column "id" "uuid"                    NotNull
--               , Column "created_at" "timestamp"       NotNull
--               , Column "user_id" "uuid"               NotNull
--               , Column "pub_id" "integer"             NotNull
--               , Column "post_id" "uuid"               NotNull
--               , Column "geo_city" "varchar(100)"      Null
--               , Column "geo_country" "varchar(100)"   Null
--               , Column "device_version" "varchar(25)" Null
--               , Column "device_name" "varchar(100)"   Null
--               , Column "device_type" "varchar(50)"    Null
--               , Column "time_spent" "integer"         NotNull
--               ]
--             , tableConstraints =
--               [ PrimaryKey "id"
--               , ForeignKey "users" [ ("user_id", "id") ]
--               , ForeignKey "publishers" [ ("pub_id", "id") ]
--               ]
--             }
-- :}
data Table = Table
             { tableName        :: !TableName         -- ^ Name of the table
             , tableColumns     :: ![Column]          -- ^ A list of the columns in the table
             , tableConstraints :: ![TableConstraint] -- ^ A list of the constraints on the table
             } deriving (Eq)

instance Show Table where
  show Table {..} =
    unlines $ ("Table " ++ Text.unpack tableName) : map show tableColumns ++ map show tableConstraints

-- | Type of a 'FactColumnType'
data FactColumnKind =
    FCKNone              -- ^ A FactColumnType without any parameters
  | FCKTargetTable       -- ^ A FactColumnType with 'factColTargetTable' as the only parameter
  | FCKMaybeSourceColumn -- ^ A FactColumnType with 'factColMaybeSourceColumn' as the only parameter
  | FCKSourceColumn      -- ^ A FactColumnType with 'factColSourceColumn' as the only parameter

#if MIN_VERSION_base(4,9,0)
-- | Type of a fact column
#else
-- | Type of a fact column
--
--   'DimTime':
--   A fact column which contains a time dimension data (e.g. `created_at`). This is not exatracted
--   as a dimension table and instead just converted to an int which depends on 'settingTimeUnit'.
--   Every fact must have one of this.
--
--   'NoDimId':
--   A fact column which contains an id of a dimension which does not need to extracted as a table
--   and does not exist already.
--
--   'TenantId':
--   A fact column which constains an id of a tenant in a multi-tenant database (e.g. `organization_id`).
--   This is not extracted as a dimension table.
--
--   'DimId':
--   A fact column which constains an id of a dimension which does not need to extracted as a table
--   and exists already.
--
--   'DimVal':
--   A fact column which constains a value which needs to be extracted to a dimension table.
--   Multiple DimVal fact columns can be extracted to the same dimension table.
--
--  'FactCount':
--  A fact column which will contain the count of the rows (@count(*)@) or count of a source column
--  if provided.
--
--  'FactCountDistinct':
--  A fact column which will contain the count of the unique values of a source column if provided
--  or the primary key of the table.
--
--  'FactSum':
--  A fact column which will contain the sum of the values of the provided source column.
--
--  'FactAverage':
--  A fact column which will contain the average of the values of the provided source column.
--
--  'FactMax':
--  A fact column which will contain the maximum of the values of the provided source column.
--
--  'FactMin':
--  A fact column which will contain the minimum of the values of the provided source column.
#endif
data FactColumnType (a :: FactColumnKind) where

#if MIN_VERSION_base(4,9,0)
  -- | A fact column which contains a time dimension data (e.g. `created_at`). This is not exatracted
  --   as a dimension table and instead just converted to an int which depends on 'settingTimeUnit'.
  --   Every fact must have one of this.
#endif
  DimTime :: FactColumnType 'FCKNone

#if MIN_VERSION_base(4,9,0)
  -- | A fact column which contains an id of a dimension which does not need to extracted as a table
  --   and does not exist already.
#endif
  NoDimId :: FactColumnType 'FCKNone

#if MIN_VERSION_base(4,9,0)
  -- | A fact column which constains an id of a tenant in a multi-tenant database (e.g. `organization_id`).
  --   This is not extracted as a dimension table.
#endif
  TenantId :: FactColumnType 'FCKNone

#if MIN_VERSION_base(4,9,0)
  -- | A fact column which constains an id of a dimension which does not need to extracted as a table
  --   and exists already.
#endif
  DimId :: { factColTargetTable  :: !TableName -- ^ Name of the target dimension table
           } -> FactColumnType 'FCKTargetTable

#if MIN_VERSION_base(4,9,0)
  -- | A fact column which constains a value which needs to be extracted to a dimension table.
  --   Multiple DimVal fact columns can be extracted to the same dimension table.
#endif
  DimVal :: { factColTargetTable  :: !TableName } -> FactColumnType 'FCKTargetTable

#if MIN_VERSION_base(4,9,0)
  -- | A fact column which will contain the count of the rows (@count(*)@) or count of a source column
  --   if provided.
#endif
  FactCount :: { factColMaybeSourceColumn :: !(Maybe ColumnName) -- ^ Name of the optional source column
                       } -> FactColumnType 'FCKMaybeSourceColumn

#if MIN_VERSION_base(4,9,0)
  -- | A fact column which will contain the count of the unique values of a source column if provided
  --   or the primary key of the table.
#endif
  FactCountDistinct :: { factColMaybeSourceColumn :: !(Maybe ColumnName) } -> FactColumnType 'FCKMaybeSourceColumn

#if MIN_VERSION_base(4,9,0)
  -- | A fact column which will contain the sum of the values of the provided source column.
#endif
  FactSum :: { factColSourceColumn  :: !ColumnName -- ^ Name of the source column
                       } -> FactColumnType 'FCKSourceColumn

#if MIN_VERSION_base(4,9,0)
  -- | A fact column which will contain the average of the values of the provided source column.
#endif
  FactAverage :: { factColSourceColumn  :: !ColumnName } -> FactColumnType 'FCKSourceColumn

#if MIN_VERSION_base(4,9,0)
  -- | A fact column which will contain the maximum of the values of the provided source column.
#endif
  FactMax :: { factColSourceColumn  :: !ColumnName } -> FactColumnType 'FCKSourceColumn

#if MIN_VERSION_base(4,9,0)
  -- | A fact column which will contain the minimum of the values of the provided source column.
#endif
  FactMin :: { factColSourceColumn  :: !ColumnName } -> FactColumnType 'FCKSourceColumn

deriving instance Show (FactColumnType a)

-- | A column in a fact table
data FactColumn = forall a. FactColumn
                  { -- | Name of the fact column in the generated table
                    factColTargetColumn :: !ColumnName
                    -- | Type of the fact column
                  , factColType         :: FactColumnType a }

deriving instance Show FactColumn

-- | A fact is a table that records measurements or metrics for a specific event
--
-- The following represents a fact for the same multi-publisher blog system:
--
-- >>> :{
-- let postFact =
--       Fact { factName            = "post_views"
--            , factTableName       = "post_view_events"
--            , factTablePersistent = True
--            , factParentNames     = []
--            , factColumns         =
--              [ FactColumn "created_at"       $ DimTime
--              , FactColumn "publisher_id"     $ TenantId
--              , FactColumn "user_id"          $ DimId "users"
--              , FactColumn "post_id"          $ NoDimId
--              , FactColumn "geo_city"         $ DimVal "geo"
--              , FactColumn "geo_country"      $ DimVal "geo"
--              , FactColumn "device_name"      $ DimVal "device"
--              , FactColumn "device_type"      $ DimVal "device"
--              , FactColumn "count"            $ FactCount Nothing
--              , FactColumn "unq_device_count" $ FactCountDistinct $ Just "device_name"
--              , FactColumn "time_spent"       $ FactSum "time_spent"
--              , FactColumn "max_time_spent"   $ FactMax "time_spent"
--              ]
--            }
-- :}
data Fact = Fact
            { -- | Name of the fact
              factName            :: !TableName
              -- | Name of the table from which the fact is derived
            , factTableName       :: !TableName
              -- | If true, the generated fact table is actually created; if false, the generated
              --   fact table is just used for intermidiate computations and is not actually created
            , factTablePersistent :: !Bool
              -- | Names of the parent facts
            , factParentNames     :: ![TableName]
              -- | A list of fact columns in the fact
            , factColumns         :: ![FactColumn]
            } deriving (Show)

-- | Returns the name of the optional source column of a fact column
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

-- | Units of time
data TimeUnit = Second | Minute | Hour | Day | Week
                deriving (Eq, Enum, Show, Read)

-- | Returns the name of a time unit
timeUnitName :: TimeUnit -> Text
timeUnitName = Text.toLower . Text.pack . show

-- | Returns the number of seconds in a time unit
timeUnitToSeconds :: TimeUnit -> Int
timeUnitToSeconds Second = 1
timeUnitToSeconds Minute = 60 * timeUnitToSeconds Second
timeUnitToSeconds Hour   = 60 * timeUnitToSeconds Minute
timeUnitToSeconds Day    = 24 * timeUnitToSeconds Hour
timeUnitToSeconds Week   = 7  * timeUnitToSeconds Day

-- | Global settings for the library
data Settings = Settings
                { -- | Prefix for the names of the generated dimension tables. Default: "dim_".
                  settingDimPrefix                  :: !Text
                  -- | Prefix for the names of the generated fact tables. Default: "fact_".
                , settingFactPrefix                 :: !Text
                  -- | Infix for the names of the generated fact tables. Default: "_by_".
                , settingFactInfix                  :: !Text
                  -- | Time unit used to summarize the fact table data. Default: 'Minute'.
                , settingTimeUnit                   :: !TimeUnit
                  -- | Suffix for the names of the generated average-count fact columns. Default: "_count".
                , settingAvgCountColumnSuffix       :: !Text
                  -- | Suffix for the names of the generated average-sum fact columns. Default: "_sum".
                , settingAvgSumColumnSuffix         :: !Text
                  -- | Name of the id columns of the generated dimension tables. Default: "id".
                , settingDimTableIdColumnName       :: !Text
                  -- | Type of the id columns of the generated dimension tables. Default: "serial".
                , settingDimTableIdColumnType       :: !Text
                  -- | Type of the count fact columns of the generated dimension tables. Default: "integer".
                , settingFactCountColumnType        :: !Text
                  -- | Maximum error rate for the hyperloglog algorithm for computing
                  --   count distinct fact columns of the generated dimension tables. Default: 0.05.
                , settingFactCountDistinctErrorRate :: !Double
                  -- | Name of the generated JSON file containing the dependency graph.
                  --   Default: "dependencies.json".
                , settingDependenciesJSONFileName   :: !Text
                  -- | Name of the generated JSON file containing the list of name of the generated
                  --   fact tables. Default: "facts.json".
                , settingFactsJSONFileName          :: !Text
                  -- | Name of the generated JSON file containing the list of name of the generated
                  --   dimension tables. Default: "dimensions.json".
                , settingDimensionsJSONFileName     :: !Text
                  -- | Value to coalesce the missing foreign key id column values to in the generated
                  --   fact tables. Default: -1.
                , settingForeignKeyIdCoalesceValue  :: !Int
                  -- | Suffix template for names of all the generated tables. Default: "{{suff}}".
                , settingTableNameSuffixTemplate    :: !Text
                } deriving (Eq, Show)

-- | Settings with default values
defSettings :: Settings
defSettings = Settings
              { settingDimPrefix                  = "dim_"
              , settingFactPrefix                 = "fact_"
              , settingFactInfix                  = "_by_"
              , settingTimeUnit                   = Minute
              , settingAvgCountColumnSuffix       = "_count"
              , settingAvgSumColumnSuffix         = "_sum"
              , settingDimTableIdColumnName       = "id"
              , settingDimTableIdColumnType       = "serial"
              , settingFactCountColumnType        = "integer"
              , settingFactCountDistinctErrorRate = 0.05
              , settingDependenciesJSONFileName   = "dependencies.json"
              , settingFactsJSONFileName          = "facts.json"
              , settingDimensionsJSONFileName     = "dimensions.json"
              , settingForeignKeyIdCoalesceValue  = -1
              , settingTableNameSuffixTemplate    = "{{suff}}"
              }

-- | Errors possible while validating the config
data ValidationError =
    -- | When referencing a table which is missing from the config
    MissingTable             !TableName
    -- | When referencing a fact which is missing from the config
  | MissingFact              !TableName
    -- | When referencing a column which is missing from the config
  | MissingColumn            !TableName !ColumnName
    -- | When a fact has no 'DimTime' columns
  | MissingTimeColumn        !TableName
    -- | When a 'DimTime' fact column of a fact is nullable
  | MissingNotNullConstraint !TableName !ColumnName
    -- | When the default value of a type is missing from the config
  | MissingTypeDefault       !Text
    -- | When there are multiple tables with the same name in the config
  | DuplicateTable           !TableName
    -- | When there are multiple facts with the same name in the config
  | DuplicateFact            !TableName
    -- | When there are multiple columns with the same name in a table in the config
  | DuplicateColumn          !TableName !ColumnName
    -- | When there are multiple dimensions with the same name in the config
  | DuplicateDimension       !TableName
  deriving (Eq, Show)

-- | A mapping of SQL types to their default values used to coleasce null column values in
--   the generated dimension and fact tables
type TypeDefaults = Map Text Text

-- | The config for the library
data Config = Config
              { _configTables       :: ![Table]
              , _configFacts        :: ![Fact]
              , _configSettings     :: !Settings
              , _configTypeDefaults :: !TypeDefaults
              } deriving (Show)

-- | Return the list of source tables from the config
configTables       :: Config -> [Table]
configTables       = _configTables

-- | Return the list of facts to be generated from the config
configFacts        :: Config -> [Fact]
configFacts        = _configFacts

-- | Return the settings from the config
configSettings     :: Config -> Settings
configSettings     = _configSettings

-- | Return the defaults for the SQL types from the config
configTypeDefaults :: Config -> TypeDefaults
configTypeDefaults = _configTypeDefaults

-- | The mode for population of the generated tables; used to switch the SQL for table population
data TablePopulationMode = FullPopulation        -- ^ Populating the tables fully, starting with empty ones
                         | IncrementalPopulation -- ^ Populating the tables incrementally
                         deriving (Eq, Show)

-- | The dependency graph of the generated tables describing the order in which they have to be populated
type Dependencies = Map TableName [TableName]
