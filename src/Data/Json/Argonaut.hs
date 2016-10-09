{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Json.Argonaut where

import Control.Applicative((<|>))
import Text.Parser.Char
import Data.Text(Text)
import Papa

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedStrings
-- >>> import Data.Either(isLeft)
-- >>> import Data.Text(pack)
-- >>> import Data.Text.Arbitrary
-- >>> import Text.Parsec(Parsec, ParseError, parse)
-- >>> import Test.QuickCheck(Arbitrary(..))
-- >>> let testparse :: Parsec Text () a -> Text -> Either ParseError a; testparse p = parse p "test"
-- >>> let testparsej :: Parsec Text () (Json ()) -> Text -> Either ParseError (Json()); testparsej = testparse

newtype JNumber =
  JNumber {
    _jnumber ::
      Double
  } deriving (Eq, Ord, Show)

data JAssoc s =
  JAssoc {
    _key ::
      Text
  , _value ::
      Json s
  }
  deriving (Eq, Ord, Show)

newtype JObject s =
  JObject {
    _jobjectL ::
      [JAssoc s]
  } deriving (Eq, Ord, Show)

data Json s =
  JsonNull s
  | JsonBool Bool s
  | JsonNumber JNumber s
  | JsonString Text s
  | JsonArray (Jsons s) s
  | JsonObject (JObject s) s
  deriving (Eq, Ord, Show)

newtype Jsons s =
  Jsons {
    _jsonsL ::
      [Json s]
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
-- >>> testparsej (parseJsonNull (return ())) "null" 
-- Right (JsonNull ())
--
-- prop> x /= "null" ==> isLeft (testparsej (parseJsonNull (return ())) x)
parseJsonNull ::
  (AsJson a s, CharParsing f) =>
  f s
  -> f a
parseJsonNull p =
  (_JsonNull #) <$ text "null" <*> p

-- |
--
-- >>> testparsej (parseJsonBool (return ())) "true" 
-- Right (JsonBool True ())
--
-- >>> testparsej (parseJsonBool (return ())) "false" 
-- Right (JsonBool False ())
--
-- prop> (x `notElem` ["true", "false"]) ==> isLeft (testparsej (parseJsonBool (return ())) x)
parseJsonBool ::
  (AsJson a s, CharParsing f) =>
  f s
  -> f a
parseJsonBool p =
  let b q t = (\r -> _JsonBool # (q, r)) <$ text t <*> p
  in  b False "false" <|> b True "true"
