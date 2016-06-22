{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Ringo.Types
  ( ColumnName, ColumnType, TableName
  , Nullable(..), Column(..), TableConstraint(..), Table(..)
  , TimeUnit(..), timeUnitName, timeUnitToSeconds
  , Fact(..), FactColumnType(..), FactColumn(..), factSourceColumnName
  , Settings(..), defSettings
  , ValidationError(..), TypeDefaults
  , Env, envTables, envFacts, envSettings, envTypeDefaults,
  TablePopulationMode(..), Dependencies) where

import Ringo.Types.Internal
