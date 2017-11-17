{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ringo.ArgParser (ProgArgs(..), parseArgs) where

import qualified Data.Text as Text

import Data.List           (intercalate)
import Data.Monoid         ((<>))
import Options.Applicative
import System.Environment  (getProgName)

import Ringo.Types

data ProgArgs = ProgArgs
                { progSettings  :: Settings
                , progInputFile :: FilePath
                , progOutputDir :: FilePath
                } deriving (Eq, Show)

textOption :: Mod OptionFields String -> Parser Text.Text
textOption o = Text.pack <$> strOption o

settingsParser :: Parser Settings
settingsParser = let Settings {..} = defSettings
  in Settings
     <$> textOption (long "dim-prefix"
                     <> short 'd'
                     <> value (Text.unpack settingDimPrefix)
                     <> showDefault
                     <> help "Prefix for dimension tables")
     <*> textOption (long "fact-prefix"
                     <> short 'f'
                     <> value (Text.unpack settingFactPrefix)
                     <> showDefault
                     <> help "Prefix for fact tables")
     <*> minorOption "fact-infix"
                     settingFactInfix
                     "Infix for fact tables"
     <*> option auto (let timeunits = map show [Second ..]
                      in long "timeunit"
                         <> short 't'
                         <> value settingTimeUnit
                         <> showDefault
                         <> completeWith timeunits
                         <> help ("Time unit granularity for fact tables. Possible values: "
                                    ++ intercalate ", " timeunits))
     <*> minorOption "avg-count-col-suffix"
                     settingAvgCountColumnSuffix
                     "Suffix for average count columns"
     <*> minorOption "avg-sum-col-suffix"
                     settingAvgSumColumnSuffix
                     "Suffix for average sum columns"
     <*> minorOption "dim-id-col-name"
                     settingDimTableIdColumnName
                     "Name of dimension table id columns"
     <*> minorOption "dim-id-col-type"
                     settingDimTableIdColumnType
                     "Type of dimension table id columns"
     <*> minorOption "fact-count-col-type"
                     settingFactCountColumnType
                     "Type of fact table count columns"
     <*> option auto (long "fact-count-distinct-error-rate"
                      <> hidden
                      <> value settingFactCountDistinctErrorRate
                      <> showDefault
                      <> help "Error rate for count distinct calulations")
     <*> minorOption "dependencies-json-file"
                     settingDependenciesJSONFileName
                     "Name of the output dependencies json file"
     <*> minorOption "facts-json-file"
                     settingFactsJSONFileName
                     "Name of the output facts json file"
     <*> minorOption "dimensions-json-file"
                     settingDimensionsJSONFileName
                     "Name of the output dimensions json file"
     <*> option auto (long "foreign-key-id-coalesce-val"
                      <> hidden
                      <> value settingForeignKeyIdCoalesceValue
                      <> showDefault
                      <> help "Value to coalease missing foriegn key ids to, in fact tables")
     <*> minorOption "tablename-suffix-template"
                     settingTableNameSuffixTemplate
                     "Suffix template for table names in SQL"
  where
    minorOption longDesc defValue helpTxt =
      textOption (long longDesc
                  <> hidden
                  <> value (Text.unpack defValue)
                  <> showDefault
                  <> help helpTxt)

progArgsParser :: Parser ProgArgs
progArgsParser =
  ProgArgs
  <$> settingsParser
  <*> argument str (metavar "INPUT"
                    <> action "file"
                    <> help "Input file")
  <*> argument str (metavar "OUTPUT"
                    <> action "directory"
                    <> help "Output directory")

versionParser :: String -> Parser (a -> a)
versionParser progName = infoOption (progName ++ " " ++ version)
  (long "version"
   <> short 'v'
   <> help "Print version information")
  where
    version = "0.1.0"

parseArgs :: IO ProgArgs
parseArgs = do
  progName <- getProgName
  execParser $
    info (helper <*> versionParser progName <*> progArgsParser)
         (fullDesc
          <> progDesc "Tool to transform Postgres OLTP schemas to OLAP star schemas automatically"
          <> header (progName ++ " - OLTP to OLAP schema transformer for Postgres")
          <> footer "© 2015-2017 Quintype Inc, Nilenso Software LLP, Abhinav Sarkar. http://github.com/abhin4v/ringo")
