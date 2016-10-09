{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map             as Map
import qualified Data.Text            as Text

import Data.Aeson       (encode)
import Data.Char        (toLower)
import Control.Monad    (forM_)
import System.Directory (createDirectoryIfMissing)
import System.FilePath  ((</>), (<.>))
import System.Exit      (exitFailure, exitSuccess)

import Ringo
import Ringo.ArgParser
import Ringo.InputParser

data SQLType = Create | FullRefresh | IncRefresh deriving (Eq, Show)

main :: IO ()
main = do
  ProgArgs {..} <- parseArgs
  result        <- parseInput progInputFile
  case result of
    Left err                        -> putStrLn err >> exitFailure
    Right (tables, facts, defaults) ->
      case makeConfig tables facts progSettings defaults of
        Left errors  -> mapM_ print errors              >> exitFailure
        Right config -> writeFiles progOutputDir config >> exitSuccess

writeFiles :: FilePath -> Config -> IO ()
writeFiles outputDir config = do
  let Settings{..} = configSettings config

  forM_ (makeSQLs config dimTables factTables) $ \(sqlType, table, sql) -> do
    let dirName = outputDir </> map toLower (show sqlType)
    createDirectoryIfMissing True dirName
    writeFile (dirName </> Text.unpack table <.> "sql") sql

  BS.writeFile (outputDir </> Text.unpack settingDependenciesJSONFileName)
    . encode
    . foldl (\acc -> Map.union acc . extractDependencies config) Map.empty
    $ facts

  BS.writeFile (outputDir </> Text.unpack settingDimensionsJSONFileName) . encode $
    [ tableName table | (_, tabs) <- dimTables, table <- tabs , table `notElem` tables ]

  BS.writeFile (outputDir </> Text.unpack settingFactsJSONFileName) . encode $
    [ tableName table | (_, table) <- factTables ]
  where
    facts      = configFacts config
    tables     = configTables config

    dimTables  = [ (fact, extractDimensionTables config fact) | fact <- facts ]
    factTables = [ (fact, extractFactTable config fact)       | fact <- facts, factTablePersistent fact ]

makeSQLs :: Config -> [(Fact, [Table])] -> [(Fact, Table)] -> [(SQLType, TableName, String)]
makeSQLs config dimTables factTables = let
    tables = configTables config

    dimTableDefinitionSQLs =
      [ (Create, tableName table, unlines . map Text.unpack $ dimensionTableDefinitionSQL config table)
         | (_, tabs) <- dimTables
         , table     <- tabs
         , table `notElem` tables ]

    factTableDefinitionSQLs =
      [ (Create , tableName table, unlines . map Text.unpack $ factTableDefinitionSQL config fact table)
        | (fact, table) <- factTables ]

    dimTablePopulationSQLs typ gen  =
      [ (typ , tableName table, Text.unpack $ gen config fact (tableName table))
        | (fact, tabs) <- dimTables
        , table        <- tabs
        , table `notElem` tables ]

    factTablePopulationSQLs typ gen = [ (typ, tableName table, unlines . map Text.unpack $ gen config fact)
                                        | (fact, table) <- factTables ]
  in concat [ dimTableDefinitionSQLs
            , factTableDefinitionSQLs
            , dimTablePopulationSQLs FullRefresh  $ dimensionTablePopulationSQL FullPopulation
            , dimTablePopulationSQLs IncRefresh   $ dimensionTablePopulationSQL IncrementalPopulation
            , factTablePopulationSQLs FullRefresh $ factTablePopulationSQL FullPopulation
            , factTablePopulationSQLs IncRefresh  $ factTablePopulationSQL IncrementalPopulation
            ]
