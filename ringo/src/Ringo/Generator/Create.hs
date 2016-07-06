{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ringo.Generator.Create ( dimensionTableDefinitionSQL
                              , dimensionTableDefinitionStatements
                              , factTableDefinitionSQL
                              , factTableDefinitionStatements
                              ) where

import Prelude.Compat
import Control.Monad.Reader     (Reader, asks)
import Database.HsSqlPpp.Syntax ( Statement(..), RowConstraint(..), AlterTableAction(..)
                                , AlterTableOperation(..), Constraint(..), Cascade(..)
                                , Replace(..) )
import Data.Maybe               (listToMaybe, maybeToList)
import Data.Monoid              ((<>))
import Data.Text                (Text)

import Ringo.Extractor.Internal
import Ringo.Generator.Sql
import Ringo.Types.Internal
import Ringo.Utils

tableDefinitionStatements :: Table -> Reader Env [Statement]
tableDefinitionStatements Table {..} = do
  Settings {..} <- asks envSettings
  let tabName  = tableName <> settingTableNameSuffixTemplate

      tableSQL = CreateTable ea (name tabName) (map columnDefnSQL tableColumns) [] Nothing NoReplace

      columnDefnSQL Column {..} =
        attDef columnName columnType $ nullableDefnSQL columnNullable

      nullableDefnSQL Null    = NullConstraint ea ""
      nullableDefnSQL NotNull = NotNullConstraint ea ""

      constraintDefnSQL constraint =
        let constr = case constraint of
              PrimaryKey cName -> PrimaryKeyConstraint ea "" [nmc cName]
              ForeignKey oTableName cNamePairs ->
                ReferenceConstraint ea "" (map (nmc . fst) cNamePairs)
                  (name oTableName) (map (nmc . snd) cNamePairs) Restrict Restrict
              UniqueKey cNames -> UniqueConstraint ea "" $ map nmc cNames

        in AlterTable ea (name tabName) $ AlterTableActions ea [AddConstraint ea constr]

  return $ tableSQL : map constraintDefnSQL tableConstraints

tableDefinitionSQL :: Table -> (Table -> Reader Env [Statement]) -> Reader Env [Text]
tableDefinitionSQL table indexFn = do
  ds <- map ppStatement <$> tableDefinitionStatements table
  is <- map (\st -> ppStatement st <> ";\n") <$> indexFn table
  return $ ds ++ is

dimensionTableDefinitionSQL :: Table -> Reader Env [Text]
dimensionTableDefinitionSQL table = tableDefinitionSQL table dimensionTableIndexStatements

dimensionTableDefinitionStatements :: Table -> Reader Env [Statement]
dimensionTableDefinitionStatements table =
  (++) <$> tableDefinitionStatements table <*> dimensionTableIndexStatements table

dimensionTableIndexStatements :: Table -> Reader Env [Statement]
dimensionTableIndexStatements Table {..} = do
  Settings {..} <- asks envSettings
  let tabName        = tableName <> settingTableNameSuffixTemplate
      tablePKColName = head [ cName | PrimaryKey cName <- tableConstraints ]
      nonPKColNames  = [ cName | Column cName _ _ <- tableColumns, cName /= tablePKColName ]

  return [ CreateIndexTSQL ea (nmc "") (name tabName) [nmc cName]
           | cName <- nonPKColNames, length nonPKColNames > 1 ]

factTableDefinitionSQL :: Fact -> Table -> Reader Env [Text]
factTableDefinitionSQL fact table = tableDefinitionSQL table (factTableIndexStatements fact)

factTableDefinitionStatements :: Fact -> Table -> Reader Env [Statement]
factTableDefinitionStatements fact table =
  (++) <$> tableDefinitionStatements table <*> factTableIndexStatements fact table

factTableIndexStatements :: Fact -> Table -> Reader Env [Statement]
factTableIndexStatements fact table = do
  allDims       <- extractAllDimensionTables fact
  Settings {..} <- asks envSettings
  tables        <- asks envTables

  let dimTimeCol           = head [ cName | FactColumn cName DimTime <- factColumns fact ]
      tenantIdCol          = listToMaybe [ cName | FactColumn cName TenantId <- factColumns fact ]
      tabName              = tableName table <> settingTableNameSuffixTemplate
      dimTimeColName cName = timeUnitColumnName settingDimTableIdColumnName cName settingTimeUnit

      factCols = forMaybe (factColumns fact) $ \FactColumn {factColTargetColumn = cName, ..} ->
        case factColType of
          DimTime   -> Just [dimTimeColName cName]
          NoDimId   -> Just [cName]
          TenantId  -> Just [cName]
          _               -> Nothing

      dimCols  = [ [ factDimFKIdColumnName settingDimPrefix settingDimTableIdColumnName dimFact dimTable tables ]
                   | (dimFact, dimTable) <- allDims ]

  return [ CreateIndexTSQL ea (nmc "") (name tabName) (map nmc cols)
           | cols <- factCols ++ dimCols ++ [ [cName, dimTimeColName dimTimeCol]
                                                       | cName <- maybeToList tenantIdCol ] ]
