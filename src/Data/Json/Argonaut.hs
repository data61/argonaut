{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE RankNTypes #-}

module Data.Json.Argonaut where

import Control.Applicative as Applicative((*>), (<*))
import Control.Applicative(Alternative((<|>), many))
import Data.Foldable(asum)
import Text.Parser.Char
import Text.Parser.Combinators
import Data.Text(Text)
import Papa

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedStrings
-- >>> import Control.Applicative as Applicative((<*))
-- >>> import Data.Either(isLeft)
-- >>> import Data.Text(pack)
-- >>> import Data.Text.Arbitrary
-- >>> import Text.Parsec(Parsec, ParseError, parse)
-- >>> import Test.QuickCheck(Arbitrary(..))
-- >>> let testparse :: Parsec Text () a -> Text -> Either ParseError a; testparse p = parse p "test"
-- >>> let testparsetheneof :: Parsec Text () a -> Text -> Either ParseError a; testparsetheneof p = testparse (p Applicative.<* eof)
-- >>> let testparsethennoteof :: Parsec Text () a -> Text -> Either ParseError a; testparsethennoteof p = testparse (p Applicative.<* anyChar)

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

data JCharEscaped =
  QuotationMark
  | ReverseSolidus
  | Solidus
  | Backspace
  | FormFeed
  | LineFeed
  | CarriageReturn
  | Tab
  | Hex HexDigit4
  deriving (Eq, Ord, Show)

data JChar =
  EscapedJChar JCharEscaped
  | UnescapedJChar JCharUnescaped
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
      [(s, Json s, s)]
  } deriving (Eq, Ord, Show)


makeClassy ''HexDigit
makeClassyPrisms ''HexDigit
makeClassy ''HexDigit4
makeClassy ''JCharEscaped
makeClassyPrisms ''JCharEscaped
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
-- >>> testparsetheneof (parseJsonNull (return ())) "null" 
-- Right (JsonNull ())
--
-- >>> testparsethennoteof (parseJsonNull (return ())) "nullx" 
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
---
-- >>> testparsetheneof (parseJsonBool (return ())) "true" 
-- Right (JsonBool True ())
--
-- >>> testparsetheneof (parseJsonBool (return ())) "false" 
-- Right (JsonBool False ())
---
-- >>> testparsethennoteof (parseJsonBool (return ())) "truex" 
-- Right (JsonBool True ())
--
-- >>> testparsethennoteof (parseJsonBool (return ())) "falsex" 
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

-- |
--
-- >>> testparse parseHexDigit "0" 
-- Right 0
--
-- >>> testparse parseHexDigit "1" 
-- Right 1
--
-- >>> testparse parseHexDigit "9" 
-- Right 9
--
-- >>> testparse parseHexDigit "a" 
-- Right a
--
-- >>> testparse parseHexDigit "f" 
-- Right f
--
-- >>> testparse parseHexDigit "A" 
-- Right A
--
-- >>> testparse parseHexDigit "F" 
-- Right F
--
-- >>> testparsetheneof parseHexDigit "F" 
-- Right F
--
-- >>> testparsethennoteof parseHexDigit "Fx" 
-- Right F
parseHexDigit ::
  CharParsing f =>
  f HexDigit
parseHexDigit =
  asum
    [
      D0 <$ char '0'
    , D1 <$ char '1'
    , D2 <$ char '2'
    , D3 <$ char '3'
    , D4 <$ char '4'
    , D5 <$ char '5'
    , D6 <$ char '6'
    , D7 <$ char '7'
    , D8 <$ char '8'
    , D9 <$ char '9'
    , Da <$ char 'a'
    , Db <$ char 'b'
    , Dc <$ char 'c'
    , Dd <$ char 'd'
    , De <$ char 'e'
    , Df <$ char 'f'
    , DA <$ char 'A'
    , DB <$ char 'B'
    , DC <$ char 'C'
    , DD <$ char 'D'
    , DE <$ char 'E'
    , DF <$ char 'F'
    ]

-- |
--
-- >>> testparse parseHexDigit4 "1234"
-- Right 1234
--
-- >>> testparse parseHexDigit4 "12aF"
-- Right 12aF
--
-- >>> testparse parseHexDigit4 "aBcD"
-- Right aBcD
--
-- >>> testparsetheneof parseHexDigit4 "12aF"
-- Right 12aF
--
-- >>> testparsethennoteof parseHexDigit4 "12aFx"
-- Right 12aF
parseHexDigit4 ::
  CharParsing f =>
  f HexDigit4
parseHexDigit4 =
  HexDigit4 <$> parseHexDigit <*> parseHexDigit <*> parseHexDigit <*> parseHexDigit

-- |
--
-- >>> testparse parseJCharUnescaped "a" 
-- Right (JCharUnescaped 'a')
--
-- >>> testparse parseJCharUnescaped "\8728" 
-- Right (JCharUnescaped '\8728')
--
-- >>> testparsetheneof parseJCharUnescaped "a" 
-- Right (JCharUnescaped 'a')
--
-- >>> testparsethennoteof parseJCharUnescaped "ax" 
-- Right (JCharUnescaped 'a')
parseJCharUnescaped ::
  CharParsing f =>
  f JCharUnescaped
parseJCharUnescaped =
  JCharUnescaped <$> satisfy (has _JCharUnescaped)

-- |
--
-- >>> testparse parseJCharEscaped "\\\""
-- Right QuotationMark
--
-- >>> testparse parseJCharEscaped "\\\\"
-- Right ReverseSolidus
--
-- >>> testparse parseJCharEscaped "\\/"
-- Right Solidus
--
-- >>> testparse parseJCharEscaped "\\b"
-- Right Backspace
--
-- >>> testparse parseJCharEscaped "\\f"
-- Right FormFeed
--
-- >>> testparse parseJCharEscaped "\\n"
-- Right LineFeed
--
-- >>> testparse parseJCharEscaped "\\r"
-- Right CarriageReturn
--
-- >>> testparse parseJCharEscaped "\\t"
-- Right Tab
--
-- >>> testparse parseJCharEscaped "\\u1234"
-- Right (Hex 1234)
--
-- >>> testparsetheneof parseJCharEscaped "\\t"
-- Right Tab
--
-- >>> testparsethennoteof parseJCharEscaped "\\tx"
-- Right Tab
parseJCharEscaped ::
  CharParsing f =>
  f JCharEscaped
parseJCharEscaped =
  let e =
        asum
          ((\(c, p) -> char c Applicative.*> pure p) <$>
            [
              ('"' , QuotationMark)
            , ('\\', ReverseSolidus)
            , ('/' , Solidus)
            , ('b' , Backspace)
            , ('f' , FormFeed)
            , ('n' , LineFeed)
            , ('r' , CarriageReturn)
            , ('t' , Tab)
            ])
      h =
        Hex <$> (char 'u' Applicative.*> parseHexDigit4)
  in  char '\\' Applicative.*> (e <|> h)

-- |
--
-- >>> testparse parseJChar "a"
-- Right (UnescapedJChar (JCharUnescaped 'a'))
--
-- >>> testparse parseJChar "\\u1234"
-- Right (EscapedJChar (Hex 1234))
--
-- >>> testparse parseJChar "\\r"
-- Right (EscapedJChar CarriageReturn)
--
-- >>> testparsetheneof parseJChar "a"
-- Right (UnescapedJChar (JCharUnescaped 'a'))
--
-- >>> testparsethennoteof parseJChar "ax"
-- Right (UnescapedJChar (JCharUnescaped 'a'))
parseJChar ::
  CharParsing f =>
  f JChar
parseJChar =
  asum
    [
      EscapedJChar <$> try parseJCharEscaped
    , UnescapedJChar <$> parseJCharUnescaped
    ]

-- |
--
-- >>> testparse parseJString "\"\""
-- Right (JString [])
--
-- >>> testparse parseJString "\"abc\""
-- Right (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')])
--
-- >> testparse parseJString "\"a\\rbc\""
-- Right (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar CarriageReturn,UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')])
--
-- >>> testparse parseJString "\"a\\rbc\\uab12\\ndef\\\"\""
-- Right (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar CarriageReturn,UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar LineFeed,UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark])
--
-- >>> testparsetheneof parseJString "\"\""
-- Right (JString [])
--
-- >>> testparsetheneof parseJString "\"abc\""
-- Right (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')])
--
-- >>> testparsethennoteof parseJString "\"a\"\\u"
-- Right (JString [UnescapedJChar (JCharUnescaped 'a')])
--
-- >>> testparsethennoteof parseJString "\"a\"\t"
-- Right (JString [UnescapedJChar (JCharUnescaped 'a')])
parseJString ::
  CharParsing f =>
  f JString
parseJString =
  char '"' Applicative.*> (JString <$> many parseJChar) Applicative.<* char '"'
