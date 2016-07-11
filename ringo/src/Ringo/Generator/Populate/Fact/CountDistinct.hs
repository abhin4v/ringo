{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}

module Ringo.Generator.Populate.Fact.CountDistinct (factCountDistinctUpdateStatements) where

import qualified Data.Text as Text

import Prelude.Compat
import Control.Monad            (forM)
import Control.Monad.Reader     (Reader, asks)
import Database.HsSqlPpp.Syntax ( QueryExpr(..), ScalarExpr, Statement, makeSelect
                                , SelectList(..), SelectItem(..) )
import Data.Maybe               (fromJust, fromMaybe, catMaybes)
import Data.Monoid              ((<>))
import Data.Text                (Text)

import Ringo.Extractor.Internal
import Ringo.Generator.Internal
import Ringo.Generator.Sql
import Ringo.Types.Internal

factCountDistinctUpdateStatements :: TablePopulationMode -> Fact -> Text -> QueryExpr -> Reader Env [Statement]
factCountDistinctUpdateStatements popMode fact groupByColPrefix expr = case expr of
  select@Select {..} -> do
    Settings {..}         <- asks envSettings
    let extFactTableName  =
          suffixTableName popMode settingTableNameSuffixTemplate
            $ extractedFactTableName settingFactPrefix settingFactInfix (factName fact) settingTimeUnit

    fmap catMaybes $ forM (factColumns fact) $ \FactColumn {factColTargetColumn = cName, ..} ->
      case factColType of
        FactCountDistinct {factColMaybeSourceColumn = scName} -> do
          let groupByCols      = map ppScalarExpr selGroupBy
          selectStmt           <- queryExpr fact cName scName groupByCols select
          let aggSelectClause  =
                sia (app "json_object_agg" [ ei (cName <> "_bnum"), ei (cName <> "_bhash") ]) (nmc cName)

          return $ Just $ update extFactTableName
            [ (cName, eqi "xyz" cName) ]
            [ subtrefa "xyz"
                makeSelect
                  { selSelectList = sl $ map (si . ei) groupByCols ++ [ aggSelectClause ]
                  , selTref       = [ subtrefa "zyx" selectStmt ]
                  , selGroupBy    = selGroupBy
                  } ] $
            foldBinop "and"
              [ binop "=" (eqi extFactTableName . fromJust . Text.stripPrefix groupByColPrefix $ col)
                           (eqi "xyz" col)
                | col <- groupByCols ]

        _ -> return Nothing

  _ -> return []

queryExpr :: Fact -> ColumnName -> Maybe ColumnName -> [ColumnName] -> QueryExpr -> Reader Env QueryExpr
queryExpr fact targetCol sourceCol groupByCols select = case select of
  Select {selSelectList = SelectList _ origSelectItems, ..} -> do
    Settings {..}      <- asks envSettings
    tables             <- asks envTables

    let fTableName     = factTableName fact
        fTable         = fromJust . findTable fTableName $ tables
        tablePKColName = head [ cName | PrimaryKey cName <- tableConstraints fTable ]
        unqCol         = cast (eqi fTableName (fromMaybe tablePKColName sourceCol)) "text"
        selectList     = [ i | i@(SelectItem _ _ a) <- origSelectItems , a `elem` map nmc groupByCols ]
    bucketSelectList   <- bucketSelectItems targetCol unqCol

    return $ makeSelect
              { selSelectList = sl $ selectList ++ bucketSelectList
              , selTref       = selTref
              , selWhere      = binop "and" (postop "isnotnull" unqCol) <$> selWhere
              , selGroupBy    = selGroupBy ++ [ ei $ targetCol <> "_bnum" ]
              }

  _ -> error "Must be a Select"

bucketSelectItems :: ColumnName -> ScalarExpr -> Reader Env [SelectItem]
bucketSelectItems targetCol unqCol = do
  Settings {..} <- asks envSettings

  return [ sia (binop "&" (app "hashtext" [ unqCol ])
                  (num . Text.pack . show $ bucketCount settingFactCountDistinctErrorRate - 1))
               (nmc $ targetCol <> "_bnum")
         , sia (binop "-"
                  (num "31")
                  (app "ilog2"
                     [ app "min" [ binop "&"
                                     (app "hashtext" [ unqCol ])
                                     (prefop "~" (parens (binop "<<" (num "1") (num "31"))))]]))
               (nmc $ targetCol <> "_bhash")
         ]
  where
    bucketCount :: Double -> Integer
    bucketCount errorRate =
      let power :: Double = fromIntegral (ceiling . logBase 2 $ (1.04 / errorRate) ** 2 :: Integer)
      in ceiling $ 2 ** power
