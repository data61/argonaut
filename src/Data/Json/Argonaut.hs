{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Json.Argonaut where

import Data.Text
import Data.Text1
import Papa

data JsonAssoc s =
  JsonAssoc {
    _key ::
      Text
  , _value ::
      Json s
  }
  deriving (Eq, Ord, Show)

newtype JsonAssocs s =
  JsonAssocs
    [JsonAssoc s]
  deriving (Eq, Ord, Show)

data Json s =
  JsonNull s
  | JsonBool Bool s
  | JsonNumber Text1 s
  | JsonString Text s
  | JsonArray (Jsons s) s
  | JsonObject (JsonAssocs s) s
  deriving (Eq, Ord, Show)

newtype Jsons s =
  Jsons
    [Json s]
  deriving (Eq, Ord, Show)

makeClassy ''JsonAssoc
makeWrapped ''JsonAssocs
makeClassy ''Json
makeClassyPrisms ''Json
makeWrapped ''Jsons
