{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ringo.Types
  ( ColumnName, ColumnType, TableName
  , Nullable(..), Column(..), TableConstraint(..), Table(..)
  , TimeUnit(..), timeUnitName, timeUnitToSeconds
  , Fact(..), FactColumnType(..), FactColumn(..), factSourceColumnName
  , Settings(..), defSettings
  , ValidationError(..), TypeDefaults
  , Env, EnvV(..), envView
  , TablePopulationMode(..), Dependencies) where

import Ringo.Types.Internal
