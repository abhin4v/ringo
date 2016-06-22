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
      case makeEnv tables facts progSettings defaults of
        Left errors -> mapM_ print errors           >> exitFailure
        Right env   -> writeFiles progOutputDir env >> exitSuccess

writeFiles :: FilePath -> Env -> IO ()
writeFiles outputDir env = do
  let Settings{..} = envSettings env

  forM_ sqls $ \(sqlType, table, sql) -> do
    let dirName = outputDir </> map toLower (show sqlType)
    createDirectoryIfMissing True dirName
    writeFile (dirName </> Text.unpack table <.> "sql") sql

  BS.writeFile (outputDir </> Text.unpack settingDependenciesJSONFileName)
    . encode
    . foldl (\acc -> Map.union acc . extractDependencies env) Map.empty
    $ facts

  BS.writeFile (outputDir </> Text.unpack settingDimensionJSONFileName) . encode $
    [ tableName table | (_, tabs) <- dimTables, table <- tabs , table `notElem` tables ]

  BS.writeFile (outputDir </> Text.unpack settingFactsJSONFileName) . encode $
    [ tableName table | (_, table) <- factTables ]

  where
    facts      = envFacts env
    tables     = envTables env

    dimTables  = [ (fact, extractDimensionTables env fact) | fact <- facts ]
    factTables = [ (fact, extractFactTable env fact)       | fact <- facts, factTablePersistent fact ]

    dimTableDefnSQLs    = [ (Create, tableName table, unlines . map sqlStr $ dimensionTableDefnSQL env table)
                            | (_, tabs) <- dimTables
                            , table     <- tabs
                            , table `notElem` tables ]

    factTableDefnSQLs   = [ (Create , tableName table, unlines . map sqlStr $ factTableDefnSQL env fact table)
                            | (fact, table) <- factTables ]

    dimTablePopulateSQLs typ gen  =
      [ (typ , tableName table, sqlStr $ gen env fact (tableName table))
        | (fact, tabs) <- dimTables
        , table        <- tabs
        , table `notElem` tables ]

    factTablePopulateSQLs typ gen = [ (typ, tableName table, unlines . map sqlStr  $ gen env fact)
                                      | (fact, table) <- factTables ]

    sqls = concat [ dimTableDefnSQLs
                  , factTableDefnSQLs
                  , dimTablePopulateSQLs FullRefresh  $ dimensionTablePopulateSQL FullPopulation
                  , dimTablePopulateSQLs IncRefresh   $ dimensionTablePopulateSQL IncrementalPopulation
                  , factTablePopulateSQLs FullRefresh $ factTablePopulateSQL FullPopulation
                  , factTablePopulateSQLs IncRefresh  $ factTablePopulateSQL IncrementalPopulation
                  ]

    sqlStr = Text.unpack
