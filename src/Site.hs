{-|
Module      : Site
Description : Data structures to generate site
Copyright   : (c) Julien Grimault, 2019
License     : MIT
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Site (
    SiteInfo(..),
    SitePaths(..),
    App(..),
    Contribution(..)
) where

import Data.Aeson
import GHC.Generics

-- | Data for the website 
data SiteInfo = SiteInfo { 
    author :: String,
    title :: String,
    twitter :: String,
    github :: String,
    flickr :: String,
    apps :: [App],
    contributions :: [Contribution]
} deriving (Eq, Generic, Show, ToJSON)

-- | Gather useful paths required to build the website
data SitePaths = SitePaths {
    source :: FilePath,
    output :: FilePath
} deriving (Eq, Show)

-- | Data for an individual app
data App = App {
    title :: String,
    thumbnail :: FilePath,
    url :: String
} deriving (Eq, Generic, Show, ToJSON)

 -- | Data for an individual open source contribution
data Contribution = Contribution {
    title :: String,
    description :: String,
    url :: String
} deriving (Eq, Generic, Show, ToJSON)
