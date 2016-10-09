{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ringo.InputParser (parseInput) where

import qualified Data.Text as Text
import qualified Data.Vector as V

import Prelude.Compat
import Data.Maybe       (fromMaybe)
import Data.Vector      ((!), (!?))
import Data.Yaml hiding (Null)

import Ringo.Types

instance FromJSON Nullable where
  parseJSON (String s) = case s of
    "null"    -> pure Null
    "notnull" -> pure NotNull
    _         -> fail $ "Invalid value for nullable: " ++ Text.unpack s
  parseJSON o          = fail $ "Cannot parse nullable: " ++ show o

instance FromJSON Column where
  parseJSON (Array a)
    | V.length a >= 2 = Column <$> parseJSON (a ! 0)
                               <*> parseJSON (a ! 1)
                               <*> parseJSON (fromMaybe "null" (a !? 2))
    | otherwise       = fail "Column needs at least two elements: name and type"
  parseJSON o         = fail $ "Cannot parse column: " ++ show o

instance FromJSON TableConstraint where
  parseJSON (Object o) = do
    cType <- o .: "type"
    case cType of
      "primary" -> PrimaryKey <$> o .: "column"
      "unique"  -> UniqueKey  <$> o .: "columns"
      "foreign" -> ForeignKey <$> o .: "table" <*> o .: "columns"
      _         -> fail $ "Invalid constraint type: " ++ cType
  parseJSON o          = fail $ "Cannot parse constraint: " ++ show o

instance FromJSON Table where
  parseJSON (Object o) = Table <$> o .: "name" <*> o .: "columns" <*> o .: "constraints"
  parseJSON o          = fail $ "Cannot parse table: " ++ show o

instance FromJSON FactColumn where
  parseJSON (Object o) = do
    cType     <- o .: "type"
    let cName = o .: "column"
    case cType of
      "dimtime"           -> FactColumn <$> cName <*> pure DimTime
      "nodimid"           -> FactColumn <$> cName <*> pure NoDimId
      "tenantid"          -> FactColumn <$> cName <*> pure TenantId
      "dimid"             -> FactColumn <$> cName <*> (DimId             <$> o .: "table")
      "dimval"            -> FactColumn <$> cName <*> (DimVal            <$> o .: "table")
      "factcount"         -> FactColumn <$> cName <*> (FactCount         <$> o .:? "sourcecolumn")
      "factcountdistinct" -> FactColumn <$> cName <*> (FactCountDistinct <$> o .:? "sourcecolumn")
      "factsum"           -> FactColumn <$> cName <*> (FactSum           <$> o .: "sourcecolumn")
      "factaverage"       -> FactColumn <$> cName <*> (FactAverage       <$> o .: "sourcecolumn")
      "factmax"           -> FactColumn <$> cName <*> (FactMax           <$> o .: "sourcecolumn")
      "factmin"           -> FactColumn <$> cName <*> (FactMin           <$> o .: "sourcecolumn")
      _                   -> fail $ "Invalid fact column type: " ++ cType
  parseJSON o          = fail $ "Cannot parse fact column: " ++ show o

instance FromJSON Fact where
  parseJSON (Object o) = Fact <$> o .: "name"
                              <*> o .: "tablename"
                              <*> o .:? "persistent"  .!= True
                              <*> o .:? "parentfacts" .!= []
                              <*> o .: "columns"
  parseJSON o          = fail $ "Cannot parse fact: " ++ show o

data Input = Input [Table] [Fact] TypeDefaults deriving (Show)

instance FromJSON Input where
  parseJSON (Object o) = Input <$> o .: "tables" <*> o .: "facts" <*> o .: "defaults"
  parseJSON o          = fail $ "Cannot parse input: " ++ show o

parseInput :: FilePath -> IO (Either String ([Table], [Fact], TypeDefaults))
parseInput file = do
  result <- decodeFileEither file
  return $ case result of
    Left pe                             -> Left $ prettyPrintParseException pe
    Right (Input tables facts defaults) -> Right (tables, facts, defaults)
