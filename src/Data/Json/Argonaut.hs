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


----

data HexDigit =
  D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9
  | Da
  | Db
  | Dc
  | Dd
  | De
  | Df
  | DA
  | DB
  | DC
  | DD
  | DE
  | DF
  deriving (Eq, Ord)

instance Show HexDigit where
  show D0 =
    "0"
  show D1 =
    "1"
  show D2 =
    "2"
  show D3 =
    "3"
  show D4 =
    "4"
  show D5 =
    "5"
  show D6 =
    "6"
  show D7 =
    "7"
  show D8 =
    "8"
  show D9 =
    "9"
  show Da =
    "a"
  show Db =
    "b"
  show Dc =
    "c"
  show Dd =
    "d"
  show De =
    "e"
  show Df =
    "f"
  show DA =
    "A"
  show DB =
    "B"
  show DC =
    "C"
  show DD =
    "D"
  show DE =
    "E"
  show DF =
    "F"
    
data HexDigit4 =
  HexDigit4
    HexDigit
    HexDigit
    HexDigit
    HexDigit
  deriving (Eq, Ord)

instance Show HexDigit4 where
  show (HexDigit4 d1 d2 d3 d4) =
    concat
      [
        show d1
      , show d2
      , show d3
      , show d4
      ]

newtype JCharUnescaped =
  JCharUnescaped
    Char
  deriving (Eq, Ord, Show)

class HasJCharUnescaped a where
  jCharUnescaped ::
    Lens'
      a
      JCharUnescaped

instance HasJCharUnescaped JCharUnescaped where
  jCharUnescaped =
    id

class AsJCharUnescaped a where
  _JCharUnescaped ::
    Prism'
      a
      JCharUnescaped

instance AsJCharUnescaped JCharUnescaped where
  _JCharUnescaped =
    id

instance AsJCharUnescaped Char where
  _JCharUnescaped =
    prism'
      (\(JCharUnescaped c) -> c)
      (\c ->  if any (\f -> f c) [(== '"'), (== '\\'), (\x -> x >= '\x00' && x <= '\x1f')]
                then
                  Nothing
                else
                  Just (JCharUnescaped c)
      )

data JChar =
  QuotationMark
  | ReverseSolidus
  | Solidus
  | Backspace
  | FormFeed
  | LineFeed
  | CarriageReturn
  | Tab
  | Hex HexDigit4
  | Unescaped JCharUnescaped
  deriving (Eq, Ord, Show)

newtype JString =
  JString
    [JChar]
  deriving (Eq, Ord, Show)

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

--  http://rfc7159.net/rfc7159
data Json s =
  JsonNull s
  | JsonBool Bool s
  | JsonNumber JNumber s
  | JsonString JString s
  | JsonArray (Jsons s) s
  | JsonObject (JObject s) s
  deriving (Eq, Ord, Show)

newtype Jsons s =
  Jsons {
    _jsonsL ::
      [Json s]
  } deriving (Eq, Ord, Show)


makeClassy ''HexDigit
makeClassyPrisms ''HexDigit
makeClassy ''HexDigit4
makeClassy ''JChar
makeClassyPrisms ''JChar
makeWrapped ''JString
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
-- >>> testparse (parseJsonNull (return ())) "null" 
-- Right (JsonNull ())
--
-- prop> x /= "null" ==> isLeft (testparse (parseJsonNull (return ())) x)
parseJsonNull ::
  CharParsing f =>
  f s
  -> f (Json s)
parseJsonNull p =
  JsonNull <$ text "null" <*> p

-- |
--
-- >>> testparse (parseJsonBool (return ())) "true" 
-- Right (JsonBool True ())
--
-- >>> testparse (parseJsonBool (return ())) "false" 
-- Right (JsonBool False ())
--
-- prop> (x `notElem` ["true", "false"]) ==> isLeft (testparse (parseJsonBool (return ())) x)
parseJsonBool ::
  CharParsing f =>
  f s
  -> f (Json s)
parseJsonBool p =
  let b q t = JsonBool q <$ text t <*> p
  in  b False "false" <|> b True "true"

-- parseHexDigit, parseHexDigitCase, parseHexDigit4, parseJCharUnescaped, parseJChar
