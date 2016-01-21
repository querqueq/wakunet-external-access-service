{-# LANGUAGE TemplateHaskell #-}

module Waku.ExternalAccess.AccessLevel where

import Database.Persist.TH
import Data.Aeson.TH

import Waku.Models.ExternalAccess (AccessLevel(..))

derivePersistField "AccessLevel"

-- $(deriveJSON defaultOptions ''AccessLevel)
