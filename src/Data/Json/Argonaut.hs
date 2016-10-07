{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Json.Argonaut where

import Control.Applicative((<|>))
import Text.Parser.Char
import Data.Text(Text)
import Papa

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts
-- >>> import Data.Either(isLeft)
-- >>> import Data.Text(pack)
-- >>> import Text.Parsec(Parsec, parse)
-- >>> import Test.QuickCheck(Arbitrary(..))

newtype JNumber =
  JNumber {
    _jnumber ::
      Double
  } deriving (Eq, Ord, Show)

data JAssoc =
  JAssoc {
    _key ::
      Text
  , _value ::
      Json
  }
  deriving (Eq, Ord, Show)

newtype JObject =
  JObject {
    _jobjectL ::
      [JAssoc]
  } deriving (Eq, Ord, Show)

data Json =
  JsonNull
  | JsonBool Bool
  | JsonNumber JNumber
  | JsonString Text
  | JsonArray Jsons
  | JsonObject JObject
  deriving (Eq, Ord, Show)

newtype Jsons =
  Jsons {
    _jsonsL ::
      [Json]
  } deriving (Eq, Ord, Show)

makeClassy ''JNumber
makeWrapped ''JNumber
makeClassy ''JAssoc
makeClassy ''JObject
makeWrapped ''JObject
makeClassy ''Json
makeClassyPrisms ''Json
makeClassy ''Jsons
makeWrapped ''Jsons

-- |
--
-- >>> parse (parseJsonNull :: Parsec Text () Json) "test" (pack "null")
-- Right JsonNull
--
-- prop> x /= "null" ==> isLeft (parse (parseJsonNull :: Parsec Text () Json) "test" (pack x))
parseJsonNull ::
  (AsJson a, CharParsing f) =>
  f a
parseJsonNull =
  (_JsonNull # ()) <$ text "null"

-- |
--
-- >>> parse (parseJsonBool :: Parsec Text () Json) "test" (pack "false")
-- Right (JsonBool False)
--
-- >>> parse (parseJsonBool :: Parsec Text () Json) "test" (pack "true")
-- Right (JsonBool True)
--
-- prop> (x `notElem` ["true", "false"]) ==> isLeft (parse (parseJsonBool :: Parsec Text () Json) "test" (pack x))
parseJsonBool ::
  (AsJson a, CharParsing f) =>
  f a
parseJsonBool =
  let b q t = (_JsonBool # q) <$ text t
  in  b False "false" <|> b True "true"
