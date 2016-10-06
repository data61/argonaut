{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Json.Argonaut where

import Text.Parser.Char
import Data.Text
import Papa

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> import Data.Either(isLeft)
-- >>> import Text.Parsec(parse, char, digit)
-- >>> import Test.QuickCheck(Arbitrary(..))

newtype JNumber =
  JNumber
    Double
  deriving (Eq, Ord, Show)

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
  | JsonNumber JNumber s
  | JsonString Text s
  | JsonArray (Jsons s) s
  | JsonObject (JsonAssocs s) s
  deriving (Eq, Ord, Show)

newtype Jsons s =
  Jsons
    [Json s]
  deriving (Eq, Ord, Show)

makeWrapped ''JNumber
makeClassy ''JsonAssoc
makeWrapped ''JsonAssocs
makeClassy ''Json
makeClassyPrisms ''Json
makeWrapped ''Jsons

-- |
--
-- >>> parse (parseJsonNull (return ())) "test" "null"
-- Right (JsonNull ())
--
-- prop> x /= "null" ==> isLeft (parse (parseJsonNull (return ())) "test" x)
parseJsonNull ::
  CharParsing f =>
  f a
  -> f (Json a)
parseJsonNull p =
  JsonNull <$ text "null" <*> p
