{-# LANGUAGE OverloadedStrings #-}
module Ringo
       ( -- | The examples in this module assume the following code has been run.
         --   The :{ and :} will only work in GHCi.

         -- $setup
         module Ringo.Types
       , makeConfig
       , extractFactTable
       , extractDimensionTables
       , extractDependencies
       , dimensionTableDefinitionSQL
       , factTableDefinitionSQL
       , dimensionTablePopulationSQL
       , factTablePopulationSQL
       , dimensionTableDefinitionStatements
       , factTableDefinitionStatements
       , dimensionTablePopulationStatement
       , factTablePopulationStatements
       ) where

import Control.Monad.Reader (runReader)
import Data.Text (Text)
import Database.HsSqlPpp.Syntax ( Statement )

import Ringo.Types
import qualified Ringo.Extractor as E
import qualified Ringo.Generator as G
import qualified Ringo.Validator as V

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Ringo
-- >>> import qualified Data.Map as Map
-- >>> import qualified Data.Text as Text
-- >>> import Data.List (nub)
-- >>> import Data.Monoid ((<>))
-- >>> :{
--let sessionEventsTable =
--      Table { tableName        = "session_events"
--            , tableColumns     =
--              [ Column "id" "uuid"                                                   NotNull
--              , Column "created_at" "timestamp without time zone"                    NotNull
--              , Column "member_id" "integer"                                         Null
--              , Column "publisher_id" "integer"                                      NotNull
--              , Column "user_agent" "character varying(1024)"                        Null
--              , Column "browser_name" "character varying(50)"                        Null
--              , Column "os" "character varying(50)"                                  Null
--              , Column "geo_country_name" "character varying(50)"                    Null
--              , Column "geo_city_name" "character varying(50)"                       Null
--              , Column "geo_continent_name" "character varying(15)"                  Null
--              , Column "geo_most_specific_subdivision_name" "character varying(100)" Null
--              , Column "geo_longitude" "numeric(9,6)"                                Null
--              , Column "geo_latitude" "numeric(9,6)"                                 Null
--              , Column "geo_time_zone" "character varying(20)"                       Null
--              , Column "geo_postal_code" "character varying(20)"                     Null
--              , Column "user_agent_name" "character varying(100)"                    Null
--              , Column "user_agent_type" "character varying(15)"                     Null
--              , Column "user_agent_version" "character varying(100)"                 Null
--              , Column "user_agent_device" "character varying(15)"                   Null
--              ]
--            , tableConstraints = [ PrimaryKey "id" ]
--            }
--    sessionFact =
--      Fact { factName            = "session"
--           , factTableName       = "session_events"
--           , factTablePersistent = True
--           , factParentNames     = []
--           , factColumns         =
--             [ FactColumn "created_at" $ DimTime
--             , FactColumn "publisher_id" $ NoDimId
--             , FactColumn "browser_name" $ DimVal "user_agent"
--             , FactColumn "os" $ DimVal "user_agent"
--             , FactColumn "user_agent_name" $ DimVal "user_agent"
--             , FactColumn "geo_country_name" $ DimVal "geo"
--             , FactColumn "geo_city_name" $ DimVal "geo"
--             , FactColumn "geo_continent_name" $ DimVal "geo"
--             , FactColumn "session_count" $ FactCount Nothing
--             ]
--           }
--    pageViewEventsTable =
--      Table { tableName = "page_view_events"
--            , tableColumns =
--              [ Column "id" "uuid"                                NotNull
--              , Column "created_at" "timestamp without time zone" NotNull
--              , Column "member_id" "integer"                      Null
--              , Column "publisher_id" "integer"                   NotNull
--              , Column "device_tracker_id" "uuid"                 Null
--              , Column "session_event_id" "uuid"                  Null
--              , Column "page_type" "character varying(20)"        NotNull
--              , Column "referrer" "character varying(1024)"       Null
--              , Column "url" "character varying(1024)"            Null
--              , Column "referrer_id" "integer"                    Null
--              ]
--            , tableConstraints =
--              [ PrimaryKey "id"
--              , ForeignKey "session_events" [ ("session_event_id", "id")
--                                            , ("publisher_id", "publisher_id")
--                                            ]
--              , ForeignKey "referrers" [ ("referrer_id", "id")
--                                       , ("publisher_id", "publisher_id")
--                                       ]
--              ]
--            }
--    pageViewFact =
--      Fact { factName = "page_view"
--           , factTableName = "page_view_events"
--           , factTablePersistent = True
--           , factParentNames = [ "session" ]
--           , factColumns =
--             [ FactColumn "created_at" $ DimTime
--             , FactColumn "publisher_id" $ NoDimId
--             , FactColumn "page_type" $ DimVal "page_type"
--             , FactColumn "referrer_id" $ DimId "referrers"
--             , FactColumn "view_count" $ FactCount Nothing
--             ]
--           }
--    referrersTable =
--      Table { tableName = "referrers"
--            , tableColumns =
--              [ Column "id" "integer"                  NotNull
--              , Column "publisher_id" "integer"        NotNull
--              , Column "name" "character varying(100)" NotNull
--              ]
--            , tableConstraints =
--              [ PrimaryKey "id"
--              , UniqueKey ["publisher_id", "name"]
--              ]
--            }
--    tables       = [sessionEventsTable, pageViewEventsTable, referrersTable]
--    facts        = [sessionFact, pageViewFact]
--    typeDefaults = Map.fromList [ ("integer", "-1")
--                                , ("timestamp", "'00-00-00 00:00:00'")
--                                , ("character", "'__UNKNOWN_VAL__'")
--                                , ("uuid", "'00000000-0000-0000-0000-000000000000'::uuid")
--                                , ("boolean", "false")
--                                , ("json", "'{}'::json")
--                                , ("numeric", "-1")
--                                , ("text", "'__UNKNOWN_VAL__'")
--                                ]
--    settings     = defSettings { settingTableNameSuffixTemplate = "" }
--    config       = case makeConfig tables facts settings typeDefaults of
--                     Left errors  -> error . unlines . map show $ errors
--                     Right config -> config
-- :}

makeConfig :: [Table] -> [Fact] -> Settings -> TypeDefaults -> Either [ValidationError] Config
makeConfig = V.validateConfig

-- |
--
-- >>> print $ extractFactTable config sessionFact
-- Table fact_session_by_minute
-- Column created_at_minute_id bigint NOT NULL
-- Column publisher_id integer NOT NULL
-- Column session_count integer NOT NULL
-- Column geo_id integer NOT NULL
-- Column user_agent_id integer NOT NULL
-- UniqueKey (created_at_minute_id, publisher_id, geo_id, user_agent_id)
-- <BLANKLINE>
-- >>> print $ extractFactTable config pageViewFact
-- Table fact_page_view_by_minute
-- Column created_at_minute_id bigint NOT NULL
-- Column publisher_id integer NOT NULL
-- Column view_count integer NOT NULL
-- Column referrer_id integer NOT NULL
-- Column page_type_id integer NOT NULL
-- Column geo_id integer NOT NULL
-- Column user_agent_id integer NOT NULL
-- UniqueKey (created_at_minute_id, publisher_id, referrer_id, page_type_id, geo_id, user_agent_id)
-- <BLANKLINE>
extractFactTable :: Config -> Fact -> Table
extractFactTable config = flip runReader config . E.extractFactTable

-- |
--
-- >>> mapM_ print $ extractDimensionTables config sessionFact
-- Table dim_geo
-- Column id serial NOT NULL
-- Column country_name character varying(50) NOT NULL
-- Column city_name character varying(50) NOT NULL
-- Column continent_name character varying(15) NOT NULL
-- PrimaryKey id
-- UniqueKey (country_name, city_name, continent_name)
-- <BLANKLINE>
-- Table dim_user_agent
-- Column id serial NOT NULL
-- Column browser_name character varying(50) NOT NULL
-- Column os character varying(50) NOT NULL
-- Column name character varying(100) NOT NULL
-- PrimaryKey id
-- UniqueKey (browser_name, os, name)
-- <BLANKLINE>
-- >>> mapM_ print . filter (`notElem` tables) $ extractDimensionTables config pageViewFact
-- Table dim_page_type
-- Column id serial NOT NULL
-- Column page_type character varying(20) NOT NULL
-- PrimaryKey id
-- UniqueKey (page_type)
-- <BLANKLINE>
extractDimensionTables :: Config -> Fact -> [Table]
extractDimensionTables config = flip runReader config . E.extractDimensionTables

-- |
--
-- >>> let depsToStr = map ((\(k, vs) -> Text.unpack $ k <> ":\n  - " <> Text.intercalate "\n  - " vs)) . Map.toList
-- >>> mapM_ putStrLn . depsToStr $ extractDependencies config sessionFact
-- dim_geo:
--   - session_events
-- dim_user_agent:
--   - session_events
-- fact_session_by_minute:
--   - session_events
--   - dim_user_agent
--   - dim_geo
-- >>> mapM_ putStrLn . depsToStr $ extractDependencies config pageViewFact
-- dim_page_type:
--   - page_view_events
-- fact_page_view_by_minute:
--   - page_view_events
--   - session_events
--   - dim_page_type
--   - referrers
--   - dim_user_agent
--   - dim_geo
extractDependencies :: Config -> Fact -> Dependencies
extractDependencies config = flip runReader config . E.extractDependencies

-- |
--
-- >>> let dimTables = filter (`notElem` tables) . nub . concatMap (extractDimensionTables config) $ facts
-- >>> let sqls = map (dimensionTableDefinitionSQL config) dimTables
-- >>> mapM_ (\sqls -> mapM_ (putStr . Text.unpack) sqls >> putStrLn "--------" ) sqls
-- create table dim_geo (
--   id serial not null,
--   country_name character varying(50) not null,
--   city_name character varying(50) not null,
--   continent_name character varying(15) not null
-- )
-- ;
-- <BLANKLINE>
-- alter table dim_geo add primary key (id);
-- <BLANKLINE>
-- alter table dim_geo add unique (country_name,
--                                 city_name,
--                                 continent_name);
-- <BLANKLINE>
-- create index  on dim_geo (country_name)
-- ;
-- create index  on dim_geo (city_name)
-- ;
-- create index  on dim_geo (continent_name)
-- ;
-- --------
-- create table dim_user_agent (
--   id serial not null,
--   browser_name character varying(50) not null,
--   os character varying(50) not null,
--   name character varying(100) not null
-- )
-- ;
-- <BLANKLINE>
-- alter table dim_user_agent add primary key (id);
-- <BLANKLINE>
-- alter table dim_user_agent add unique (browser_name, os, name);
-- <BLANKLINE>
-- create index  on dim_user_agent (browser_name)
-- ;
-- create index  on dim_user_agent (os)
-- ;
-- create index  on dim_user_agent (name)
-- ;
-- --------
-- create table dim_page_type (
--   id serial not null,
--   page_type character varying(20) not null
-- )
-- ;
-- <BLANKLINE>
-- alter table dim_page_type add primary key (id);
-- <BLANKLINE>
-- alter table dim_page_type add unique (page_type);
-- <BLANKLINE>
-- --------
dimensionTableDefinitionSQL :: Config -> Table -> [Text]
dimensionTableDefinitionSQL config = flip runReader config . G.dimensionTableDefinitionSQL

dimensionTableDefinitionStatements :: Config -> Table -> [Statement]
dimensionTableDefinitionStatements config = flip runReader config . G.dimensionTableDefinitionStatements

-- |
--
-- >>> let storySessionFactTable = extractFactTable config sessionFact
-- >>> let sqls = factTableDefinitionSQL config sessionFact storySessionFactTable
-- >>> mapM_ (putStr . Text.unpack) sqls
-- create table fact_session_by_minute (
--   created_at_minute_id bigint not null,
--   publisher_id integer not null,
--   session_count integer not null,
--   geo_id integer not null,
--   user_agent_id integer not null
-- )
-- ;
-- <BLANKLINE>
-- alter table fact_session_by_minute add unique (created_at_minute_id,
--                                                publisher_id,
--                                                geo_id,
--                                                user_agent_id);
-- <BLANKLINE>
-- create index  on fact_session_by_minute (created_at_minute_id)
-- ;
-- create index  on fact_session_by_minute (publisher_id)
-- ;
-- create index  on fact_session_by_minute (geo_id)
-- ;
-- create index  on fact_session_by_minute (user_agent_id)
-- ;
-- >>> let pageViewFactTable = extractFactTable config pageViewFact
-- >>> let sqls = factTableDefinitionSQL config pageViewFact pageViewFactTable
-- >>> mapM_ (putStr . Text.unpack) sqls
-- create table fact_page_view_by_minute (
--   created_at_minute_id bigint not null,
--   publisher_id integer not null,
--   view_count integer not null,
--   referrer_id integer not null,
--   page_type_id integer not null,
--   geo_id integer not null,
--   user_agent_id integer not null
-- )
-- ;
-- <BLANKLINE>
-- alter table fact_page_view_by_minute add unique (created_at_minute_id,
--                                                  publisher_id,
--                                                  referrer_id,
--                                                  page_type_id,
--                                                  geo_id,
--                                                  user_agent_id);
-- <BLANKLINE>
-- create index  on fact_page_view_by_minute (created_at_minute_id)
-- ;
-- create index  on fact_page_view_by_minute (publisher_id)
-- ;
-- create index  on fact_page_view_by_minute (referrer_id)
-- ;
-- create index  on fact_page_view_by_minute (page_type_id)
-- ;
-- create index  on fact_page_view_by_minute (geo_id)
-- ;
-- create index  on fact_page_view_by_minute (user_agent_id)
-- ;
factTableDefinitionSQL :: Config -> Fact -> Table -> [Text]
factTableDefinitionSQL config fact = flip runReader config . G.factTableDefinitionSQL fact

factTableDefinitionStatements :: Config -> Fact -> Table -> [Statement]
factTableDefinitionStatements config fact = flip runReader config . G.factTableDefinitionStatements fact

-- |
--
-- >>> let storySessionDimTableNames = map tableName $ extractDimensionTables config sessionFact
-- >>> let sqls = map (dimensionTablePopulationSQL FullPopulation config sessionFact) storySessionDimTableNames
-- >>> mapM_ (putStr . Text.unpack) sqls
-- insert into dim_geo (country_name, city_name, continent_name)
-- select distinct
--     coalesce(session_events.geo_country_name,'__UNKNOWN_VAL__') as geo_country_name,
--     coalesce(session_events.geo_city_name,'__UNKNOWN_VAL__') as geo_city_name,
--     coalesce(session_events.geo_continent_name,'__UNKNOWN_VAL__') as geo_continent_name
--   from
--     session_events
--   where
--     (geo_country_name is not null or geo_city_name is not null or geo_continent_name is not null)
--     and
--     created_at < ?
-- ;
-- <BLANKLINE>
-- insert into dim_user_agent (browser_name, os, name)
-- select distinct
--     coalesce(session_events.browser_name,'__UNKNOWN_VAL__') as browser_name,
--     coalesce(session_events.os,'__UNKNOWN_VAL__') as os,
--     coalesce(session_events.user_agent_name,'__UNKNOWN_VAL__') as user_agent_name
--   from
--     session_events
--   where
--     (browser_name is not null or os is not null or user_agent_name is not null)
--     and
--     created_at < ?
-- ;
-- <BLANKLINE>
-- >>> let sqls = map (dimensionTablePopulationSQL IncrementalPopulation config sessionFact) storySessionDimTableNames
-- >>> mapM_ (putStr . Text.unpack) sqls
-- insert into dim_geo (country_name, city_name, continent_name)
-- select
--     x.*
--   from
--     (select distinct
--          coalesce(session_events.geo_country_name,'__UNKNOWN_VAL__') as geo_country_name,
--          coalesce(session_events.geo_city_name,'__UNKNOWN_VAL__') as geo_city_name,
--          coalesce(session_events.geo_continent_name,'__UNKNOWN_VAL__') as geo_continent_name
--        from
--          session_events
--        where
--          (geo_country_name is not null or geo_city_name is not null or geo_continent_name is not null)
--          and
--          created_at < ?
--          and
--          created_at >= ?) as x
--     left outer join
--     dim_geo
--       on dim_geo.country_name = x.geo_country_name
--          and
--          dim_geo.city_name = x.geo_city_name
--          and
--          dim_geo.continent_name = x.geo_continent_name
--   where
--     dim_geo.country_name is null and dim_geo.city_name is null
--     and
--     dim_geo.continent_name is null
-- ;
-- <BLANKLINE>
-- insert into dim_user_agent (browser_name, os, name)
-- select
--     x.*
--   from
--     (select distinct
--          coalesce(session_events.browser_name,'__UNKNOWN_VAL__') as browser_name,
--          coalesce(session_events.os,'__UNKNOWN_VAL__') as os,
--          coalesce(session_events.user_agent_name,'__UNKNOWN_VAL__') as user_agent_name
--        from
--          session_events
--        where
--          (browser_name is not null or os is not null or user_agent_name is not null)
--          and
--          created_at < ?
--          and
--          created_at >= ?) as x
--     left outer join
--     dim_user_agent
--       on dim_user_agent.browser_name = x.browser_name
--          and
--          dim_user_agent.os = x.os
--          and
--          dim_user_agent.name = x.user_agent_name
--   where
--     dim_user_agent.browser_name is null and dim_user_agent.os is null
--     and
--     dim_user_agent.name is null
-- ;
-- <BLANKLINE>
-- >>> let pageViewDimTableNames = map tableName . filter (`notElem` tables) $ extractDimensionTables config pageViewFact
-- >>> let sqls = map (dimensionTablePopulationSQL FullPopulation config pageViewFact) pageViewDimTableNames
-- >>> mapM_ (putStr . Text.unpack) sqls
-- insert into dim_page_type (page_type)
-- select distinct
--     page_view_events.page_type as page_type
--   from
--     page_view_events
--   where
--     (page_type is not null) and created_at < ?
-- ;
-- <BLANKLINE>
-- >>> let sqls = map (dimensionTablePopulationSQL IncrementalPopulation config pageViewFact) pageViewDimTableNames
-- >>> mapM_ (putStr . Text.unpack) sqls
-- insert into dim_page_type (page_type)
-- select
--     x.*
--   from
--     (select distinct
--          page_view_events.page_type as page_type
--        from
--          page_view_events
--        where
--          (page_type is not null) and created_at < ?
--          and
--          created_at >= ?) as x
--     left outer join
--     dim_page_type
--       on dim_page_type.page_type = x.page_type
--   where
--     dim_page_type.page_type is null
-- ;
-- <BLANKLINE>
dimensionTablePopulationSQL :: TablePopulationMode -> Config -> Fact -> TableName -> Text
dimensionTablePopulationSQL popMode config fact =
  flip runReader config . G.dimensionTablePopulationSQL popMode fact

dimensionTablePopulationStatement :: TablePopulationMode -> Config -> Fact -> TableName -> Statement
dimensionTablePopulationStatement popMode config fact =
  flip runReader config . G.dimensionTablePopulationStatement popMode fact

-- |
--
-- >>> let sqls = factTablePopulationSQL FullPopulation config sessionFact
-- >>> mapM_ (putStr . Text.unpack) sqls
-- insert into fact_session_by_minute (created_at_minute_id,
--                                     publisher_id,
--                                     session_count,
--                                     geo_id,
--                                     user_agent_id)
-- select
--     cast(floor(extract(epoch from session_events.created_at) / 60) as bigint) as xxff_created_at_minute_id,
--     session_events.publisher_id as xxff_publisher_id,
--     count(*) as xxff_session_count,
--     coalesce((select
--                   id
--                 from
--                   dim_geo as dim_geo
--                 where
--                   dim_geo.country_name = coalesce(session_events.geo_country_name,'__UNKNOWN_VAL__')
--                   and
--                   dim_geo.city_name = coalesce(session_events.geo_city_name,'__UNKNOWN_VAL__')
--                   and
--                   dim_geo.continent_name = coalesce(session_events.geo_continent_name,'__UNKNOWN_VAL__')),-1) as xxff_geo_id,
--     coalesce((select
--                   id
--                 from
--                   dim_user_agent as dim_user_agent
--                 where
--                   dim_user_agent.browser_name = coalesce(session_events.browser_name,'__UNKNOWN_VAL__')
--                   and
--                   dim_user_agent.os = coalesce(session_events.os,'__UNKNOWN_VAL__')
--                   and
--                   dim_user_agent.name = coalesce(session_events.user_agent_name,'__UNKNOWN_VAL__')),-1) as xxff_user_agent_id
--   from
--     session_events
--   where
--     session_events.created_at < ?
--   group by
--     xxff_created_at_minute_id,
--     xxff_publisher_id,
--     xxff_geo_id,
--     xxff_user_agent_id
-- ;
-- <BLANKLINE>
-- >>> let sqls = factTablePopulationSQL IncrementalPopulation config sessionFact
-- >>> mapM_ (putStr . Text.unpack) sqls
-- insert into fact_session_by_minute (created_at_minute_id,
--                                     publisher_id,
--                                     session_count,
--                                     geo_id,
--                                     user_agent_id)
-- select
--     cast(floor(extract(epoch from session_events.created_at) / 60) as bigint) as xxff_created_at_minute_id,
--     session_events.publisher_id as xxff_publisher_id,
--     count(*) as xxff_session_count,
--     coalesce((select
--                   id
--                 from
--                   dim_geo as dim_geo
--                 where
--                   dim_geo.country_name = coalesce(session_events.geo_country_name,'__UNKNOWN_VAL__')
--                   and
--                   dim_geo.city_name = coalesce(session_events.geo_city_name,'__UNKNOWN_VAL__')
--                   and
--                   dim_geo.continent_name = coalesce(session_events.geo_continent_name,'__UNKNOWN_VAL__')),-1) as xxff_geo_id,
--     coalesce((select
--                   id
--                 from
--                   dim_user_agent as dim_user_agent
--                 where
--                   dim_user_agent.browser_name = coalesce(session_events.browser_name,'__UNKNOWN_VAL__')
--                   and
--                   dim_user_agent.os = coalesce(session_events.os,'__UNKNOWN_VAL__')
--                   and
--                   dim_user_agent.name = coalesce(session_events.user_agent_name,'__UNKNOWN_VAL__')),-1) as xxff_user_agent_id
--   from
--     session_events
--   where
--     session_events.created_at < ? and session_events.created_at >= ?
--   group by
--     xxff_created_at_minute_id,
--     xxff_publisher_id,
--     xxff_geo_id,
--     xxff_user_agent_id
-- ;
-- <BLANKLINE>
-- >>> let sqls = factTablePopulationSQL FullPopulation config pageViewFact
-- >>> mapM_ (putStr . Text.unpack) sqls
-- insert into fact_page_view_by_minute (created_at_minute_id,
--                                       publisher_id,
--                                       view_count,
--                                       referrer_id,
--                                       page_type_id,
--                                       geo_id,
--                                       user_agent_id)
-- select
--     cast(floor(extract(epoch from page_view_events.created_at) / 60) as bigint) as xxff_created_at_minute_id,
--     page_view_events.publisher_id as xxff_publisher_id,
--     count(*) as xxff_view_count,
--     coalesce(page_view_events.referrer_id,-1) as xxff_referrer_id,
--     coalesce((select
--                   id
--                 from
--                   dim_page_type as dim_page_type
--                 where
--                   dim_page_type.page_type = page_view_events.page_type),-1) as xxff_page_type_id,
--     coalesce((select
--                   id
--                 from
--                   dim_geo as dim_geo
--                 where
--                   dim_geo.country_name = coalesce(session_events.geo_country_name,'__UNKNOWN_VAL__')
--                   and
--                   dim_geo.city_name = coalesce(session_events.geo_city_name,'__UNKNOWN_VAL__')
--                   and
--                   dim_geo.continent_name = coalesce(session_events.geo_continent_name,'__UNKNOWN_VAL__')),-1) as xxff_geo_id,
--     coalesce((select
--                   id
--                 from
--                   dim_user_agent as dim_user_agent
--                 where
--                   dim_user_agent.browser_name = coalesce(session_events.browser_name,'__UNKNOWN_VAL__')
--                   and
--                   dim_user_agent.os = coalesce(session_events.os,'__UNKNOWN_VAL__')
--                   and
--                   dim_user_agent.name = coalesce(session_events.user_agent_name,'__UNKNOWN_VAL__')),-1) as xxff_user_agent_id
--   from
--     page_view_events
--     left outer join
--     session_events
--       on page_view_events.session_event_id = session_events.id
--          and
--          page_view_events.publisher_id = session_events.publisher_id
--   where
--     page_view_events.created_at < ?
--   group by
--     xxff_created_at_minute_id,
--     xxff_publisher_id,
--     xxff_referrer_id,
--     xxff_page_type_id,
--     xxff_geo_id,
--     xxff_user_agent_id
-- ;
-- <BLANKLINE>
factTablePopulationSQL :: TablePopulationMode -> Config -> Fact -> [Text]
factTablePopulationSQL popMode config =
  flip runReader config . G.factTablePopulationSQL popMode

factTablePopulationStatements :: TablePopulationMode -> Config -> Fact -> [Statement]
factTablePopulationStatements popMode config =
  flip runReader config . G.factTablePopulationStatements popMode
