
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

{-# LANGUAGE RankNTypes             #-}

module Data.Json.Argonaut where

import           Control.Applicative     as Applicative ((*>), (<*))
import           Control.Applicative     (Alternative (many, (<|>)))
import           Data.Digit
import           Data.Foldable           (asum)
import           Data.List               (length)
import           Data.List.NonEmpty
import           Data.Maybe
import           Data.Scientific         (Scientific, scientific)
import           Data.Text               as Text (Text, pack)
import           Papa                    hiding (exp)
import           Text.Parser.Char
import           Text.Parser.Combinators

import           Data.Char               (chr)
import           Prelude                 (maxBound, minBound)

-- import qualified Prelude as Prelude(error, undefined)

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
-- >>> let testparsethen :: Parsec Text () a -> Text -> Either ParseError (a, Char); testparsethen p = parse ((,) <$> p <*> Text.Parser.Char.anyChar) "test"

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
  show (HexDigit4 q1 q2 q3 q4) =
    [
      q1
    , q2
    , q3
    , q4
    ] >>= show

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

data Digit1to9 =
  D1_1to9
  | D2_1to9
  | D3_1to9
  | D4_1to9
  | D5_1to9
  | D6_1to9
  | D7_1to9
  | D8_1to9
  | D9_1to9
  deriving (Eq, Ord, Show)

instance D9 Digit1to9 where
  d9 =
    prism'
      (\() -> D9_1to9)
      (\d -> case d of
               D9_1to9 ->
                 Just ()
               _ ->
                 Nothing)

instance D8 Digit1to9 where
  d8 =
    prism'
      (\() -> D8_1to9)
      (\d -> case d of
               D8_1to9 ->
                 Just ()
               _ ->
                 Nothing)

instance D7 Digit1to9 where
  d7 =
    prism'
      (\() -> D7_1to9)
      (\d -> case d of
               D7_1to9 ->
                 Just ()
               _ ->
                 Nothing)

instance D6 Digit1to9 where
  d6 =
    prism'
      (\() -> D6_1to9)
      (\d -> case d of
               D6_1to9 ->
                 Just ()
               _ ->
                 Nothing)

instance D5 Digit1to9 where
  d5 =
    prism'
      (\() -> D5_1to9)
      (\d -> case d of
               D5_1to9 ->
                 Just ()
               _ ->
                 Nothing)

instance D4 Digit1to9 where
  d4 =
    prism'
      (\() -> D4_1to9)
      (\d -> case d of
               D4_1to9 ->
                 Just ()
               _ ->
                 Nothing)

instance D3 Digit1to9 where
  d3 =
    prism'
      (\() -> D3_1to9)
      (\d -> case d of
               D3_1to9 ->
                 Just ()
               _ ->
                 Nothing)

instance D2 Digit1to9 where
  d2 =
    prism'
      (\() -> D2_1to9)
      (\d -> case d of
               D2_1to9 ->
                 Just ()
               _ ->
                 Nothing)

instance D1 Digit1to9 where
  d1 =
    prism'
      (\() -> D1_1to9)
      (\d -> case d of
               D1_1to9 ->
                 Just ()
               _ ->
                 Nothing)

digit1to9toDigit :: Prism' Digit Digit1to9
digit1to9toDigit = prism'
    (\x -> case x of
        D1_1to9 -> x1
        D2_1to9 -> x2
        D3_1to9 -> x3
        D4_1to9 -> x4
        D5_1to9 -> x5
        D6_1to9 -> x6
        D7_1to9 -> x7
        D8_1to9 -> x8
        D9_1to9 -> x9)
    (\x -> asum
            [ x1 <$ x ^? d1
            , x2 <$ x ^? d2
            , x3 <$ x ^? d3
            , x4 <$ x ^? d4
            , x5 <$ x ^? d5
            , x6 <$ x ^? d6
            , x7 <$ x ^? d7
            , x8 <$ x ^? d8
            , x9 <$ x ^? d9
            ]
    )


data JInt =
  JZero
  | JIntInt Digit1to9 [Digit]
  deriving (Eq, Ord, Show)

data E =
  EE
  | Ee
  deriving (Eq, Ord, Show)

newtype Frac =
  Frac
    (NonEmpty Digit)
  deriving (Eq, Ord, Show)

data Exp =
  Exp {
    _ex ::
      E
  , _minusplus ::
     Maybe Bool
  , _expdigits ::
     NonEmpty Digit
  }
  deriving (Eq, Ord, Show)

data JNumber =
  JNumber {
    _minus ::
      Bool
  , _numberint ::
      JInt
  , _frac ::
      Maybe Frac
  , _expn ::
      Maybe Exp
  }
  deriving (Eq, Ord, Show)

data JAssoc s =
  JAssoc {
    _key ::
      LeadingTrailing JString s
  , _value ::
      LeadingTrailing (Json s) s
  }
  deriving (Eq, Ord, Show)

instance Functor JAssoc where
    fmap f (JAssoc k v) = JAssoc (fmap f k) ((\x -> x{_a = fmap f (_a x)}) . fmap f $ v)

instance Foldable JAssoc where
    foldMap f (JAssoc k v) = mconcat [foldMap f k, foldMap' v] where
        foldMap' (LeadingTrailing l x r) = mconcat [f l, foldMap f x, f r]

instance Traversable JAssoc where
    traverse f (JAssoc k v) = JAssoc <$> traverse f k <*> traverse' v where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

newtype JObject s =
  JObject {
    _jobjectL ::
      [LeadingTrailing (JAssoc s) s]
  } deriving (Eq, Ord, Show)

instance Functor JObject where
    fmap f (JObject ls) = JObject (fmap fmap' ls) where
        fmap' (LeadingTrailing l x r) =
            LeadingTrailing (f l) (fmap f x) (f r)

instance Foldable JObject where
    foldMap f (JObject ls) = mconcat (fmap (foldMap f) ls)

instance Traversable JObject where
    traverse f (JObject ls) = JObject <$> traverse traverse' ls where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

data LeadingTrailing a s =
  LeadingTrailing {
    _leading ::
      s
  , _a ::
      a
  , _trailing ::
      s
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

--  http://rfc7159.net/rfc7159
data Json s =
  JsonNull s
  | JsonBool Bool s
  | JsonNumber JNumber s
  | JsonString JString s
  | JsonArray (Jsons s) s
  | JsonObject (JObject s) s
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Jsons s =
  Jsons {
    _jsonsL ::
      [LeadingTrailing (Json s) s]
  } deriving (Eq, Ord, Show)

instance Functor Jsons where
    fmap f (Jsons ls) = Jsons (fmap ((\x -> x{_a = fmap f (_a x)}) . fmap f) ls)

instance Foldable Jsons where
    foldMap f (Jsons ls) = mconcat (fmap (foldMap f) ls)

instance Traversable Jsons where
    traverse f (Jsons ls) = Jsons <$> traverse traverse' ls where
        traverse' (LeadingTrailing l x r) =
            LeadingTrailing
                <$> f l
                <*> traverse f x
                <*> f r

makeClassy ''HexDigit
makeClassyPrisms ''HexDigit
makeClassy ''HexDigit4
makeClassy ''JCharEscaped
makeClassyPrisms ''JCharEscaped
makeClassy ''JChar
makeClassyPrisms ''JChar
makeWrapped ''JString
makeClassy ''JInt
makeClassyPrisms ''JInt
makeClassy ''E
makeClassyPrisms ''E
makeWrapped ''Frac
makeClassy ''Exp
makeClassy ''JNumber
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
  in  (b False "false" <|> b True "true") <?> "bool 'true' or 'false'"

parseJsonNumber ::
  (Monad f, CharParsing f) =>
  f s
  -> f (Json s)
parseJsonNumber p =
  JsonNumber <$> parseJNumber <*> p

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
    ] <?> "hex digit [0-9a-fA-F]"

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
  JCharUnescaped <$> satisfy (has _JCharUnescaped) <?> "unescaped char"

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
  let z =
        asum
          ((\(c, p) -> (char c <?> concat [show p, " (", show c,")"]) Applicative.*> pure p) <$>
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
        Hex <$> ((char 'u' Applicative.*> parseHexDigit4) <?> "hex escaped character '\\uNNNN'")
  in  char '\\' Applicative.*> (z <|> h) <?> "escaped character"

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
      EscapedJChar <$> parseJCharEscaped
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
  (char '"' <?> "opening quote \'\"\'") Applicative.*>
  (JString <$> many parseJChar) Applicative.<*
  (char '"' <?> "closing quote \'\"\'")

-- |
--
-- >>> testparse (parseJsonString (return ())) "\"\""
-- Right (JsonString (JString []) ())
--
-- >>> testparse (parseJsonString (return ())) "\"abc\""
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >> testparse (parseJsonString (return ())) "\"a\\rbc\""
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar CarriageReturn,UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar LineFeed,UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) ())
--
-- >>> testparse (parseJsonString (return ())) "\"a\\rbc\\uab12\\ndef\\\"\""
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a'),EscapedJChar CarriageReturn,UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c'),EscapedJChar (Hex ab12),EscapedJChar LineFeed,UnescapedJChar (JCharUnescaped 'd'),UnescapedJChar (JCharUnescaped 'e'),UnescapedJChar (JCharUnescaped 'f'),EscapedJChar QuotationMark]) ())
--
-- >>> testparsetheneof (parseJsonString (return ())) "\"\""
-- Right (JsonString (JString []) ())
--
-- >>> testparsetheneof (parseJsonString (return ())) "\"abc\""
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a'),UnescapedJChar (JCharUnescaped 'b'),UnescapedJChar (JCharUnescaped 'c')]) ())
--
-- >>> testparsethennoteof (parseJsonString (return ())) "\"a\"\\u"
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a')]) ())
--
-- >>> testparsethennoteof (parseJsonString (return ())) "\"a\"\t"
-- Right (JsonString (JString [UnescapedJChar (JCharUnescaped 'a')]) ())
parseJsonString ::
  CharParsing f =>
  f s
  -> f (Json s)
parseJsonString p =
  JsonString <$> parseJString <*> p

parseJsons ::
  (Monad f, CharParsing f) =>
  f s
  -> f (Jsons s)
parseJsons s =
  Jsons <$>
    (
      (char '[' <?> "array opening \'[\'") Applicative.*>
      sepBy (parseLeadingTrailing s (parseJson s)) (char ',' <?> "array seperator ','") Applicative.<*
      (char ']' <?> "array closing ']'")
    )

parseJsonArray ::
  (Monad f, CharParsing f) =>
  f s
  -> f (Json s)
parseJsonArray p =
  JsonArray <$> parseJsons p <*> p

parseJAssoc ::
  (Monad f, CharParsing f) =>
  f s
  -> f (JAssoc s)
parseJAssoc s =
  JAssoc <$> (parseLeadingTrailing s parseJString <?> "object key")
         Applicative.<* (char ':' <?> "object seperator ':'")
         <*> parseLeadingTrailing s (parseJson s)

parseJObject ::
  (Monad f, CharParsing f) =>
  f s
  -> f (JObject s)
parseJObject s =
  JObject <$>
    (
      (char '{' <?> "object opening '{'") Applicative.*>
      sepBy (parseLeadingTrailing s (parseJAssoc s)) (char ',') Applicative.<*
      (char '}' <?> "object closing '{'")
    )

parseJsonObject ::
  (Monad f, CharParsing f) =>
  f s
  -> f (Json s)
parseJsonObject p =
  JsonObject <$> parseJObject p <*> p

parseJson ::
  (Monad f, CharParsing f) =>
  f s
  -> f (Json s)
parseJson =
  asum . sequence
    [
      (<?> "null") . parseJsonNull
    , (<?> "bool") . parseJsonBool
    , (<?> "number") . parseJsonNumber
    , (<?> "string") . parseJsonString
    , (<?> "array") . parseJsonArray
    , (<?> "object") . parseJsonObject
    ]

parseLeadingTrailing ::
  Applicative f =>
  f s
  -> f a
  -> f (LeadingTrailing a s)
parseLeadingTrailing s a =
  LeadingTrailing <$> s <*> a <*> s

----

-- |
--
-- >>> testparse parseDigit1to9 "1"
-- Right D1_1to9
--
-- >>> testparse parseDigit1to9 "9"
-- Right D9_1to9
--
-- >>> testparse parseDigit1to9 "5"
-- Right D5_1to9
--
-- >>> testparsetheneof parseDigit1to9 "1"
-- Right D1_1to9
--
-- >>> testparsetheneof parseDigit1to9 "9"
-- Right D9_1to9
--
-- >>> testparsetheneof parseDigit1to9 "5"
-- Right D5_1to9
--
-- >>> isLeft (testparsetheneof parseDigit1to9 "0")
-- True
--
-- >>> testparsethennoteof parseDigit1to9 "1a"
-- Right D1_1to9
--
-- >>> testparsethennoteof parseDigit1to9 "9a"
-- Right D9_1to9
--
-- >>> testparsethennoteof parseDigit1to9 "5a"
-- Right D5_1to9
parseDigit1to9 ::
  CharParsing f =>
  f Digit1to9
parseDigit1to9 =
  asum [
    D1_1to9 <$ char '1'
  , D2_1to9 <$ char '2'
  , D3_1to9 <$ char '3'
  , D4_1to9 <$ char '4'
  , D5_1to9 <$ char '5'
  , D6_1to9 <$ char '6'
  , D7_1to9 <$ char '7'
  , D8_1to9 <$ char '8'
  , D9_1to9 <$ char '9'
  ] <?> "digit [1-9]"

-- |
--
-- >>> testparse parseJInt "1"
-- Right (JIntInt D1_1to9 [])
--
-- >>> testparse parseJInt "9"
-- Right (JIntInt D9_1to9 [])
--
-- >>> testparse parseJInt "10"
-- Right (JIntInt D1_1to9 [0])
--
-- >>> testparse parseJInt "39"
-- Right (JIntInt D3_1to9 [9])
--
-- >>> testparse parseJInt "393564"
-- Right (JIntInt D3_1to9 [9,3,5,6,4])
--
-- >>> testparse parseJInt "0"
-- Right JZero
--
-- >>> testparsethennoteof parseJInt "00"
-- Right JZero
--
-- >>> testparsethennoteof parseJInt "01"
-- Right JZero
--
-- >>> testparsetheneof parseJInt "1"
-- Right (JIntInt D1_1to9 [])
--
-- >>> testparsetheneof parseJInt "9"
-- Right (JIntInt D9_1to9 [])
--
-- >>> testparsetheneof parseJInt "10"
-- Right (JIntInt D1_1to9 [0])
--
-- >>> testparsetheneof parseJInt "39"
-- Right (JIntInt D3_1to9 [9])
--
-- >>> testparsetheneof parseJInt "393564"
-- Right (JIntInt D3_1to9 [9,3,5,6,4])
--
-- >>> testparsetheneof parseJInt "0"
-- Right JZero
--
-- >>> isLeft (testparse parseJInt "x")
-- True
--
-- >>> isLeft (testparse parseJInt "")
-- True
parseJInt ::
  (Monad f, CharParsing f) =>
  f JInt
parseJInt =
  asum [
    JZero <$ try (char '0')
  , JIntInt <$> parseDigit1to9 <*> parsedigitlist
  ]

-- |
--
-- >>> testparse parseE "e"
-- Right Ee
--
-- >>> testparse parseE "E"
-- Right EE
--
-- >>> testparsetheneof parseE "e"
-- Right Ee
--
-- >>> testparsetheneof parseE "E"
-- Right EE
--
-- >>> isLeft (testparsetheneof parseE "x")
-- True
--
-- >>> testparsethennoteof parseE "ea"
-- Right Ee
--
-- >>> testparsethennoteof parseE "Ea"
-- Right EE
parseE ::
  CharParsing f =>
  f E
parseE =
  asum [
    Ee <$ char 'e'
  , EE <$ char 'E'
  ]

-- |
--
-- >>> testparsetheneof parseFrac "1"
-- Right (Frac (1 :| []))
--
-- >>> testparsetheneof parseFrac "9"
-- Right (Frac (9 :| []))
--
-- >>> testparsetheneof parseFrac "10"
-- Right (Frac (1 :| [0]))
--
-- >>> testparsetheneof parseFrac "39"
-- Right (Frac (3 :| [9]))
--
-- >>> testparsetheneof parseFrac "393564"
-- Right (Frac (3 :| [9,3,5,6,4]))
--
-- >>> testparsetheneof parseFrac "0"
-- Right (Frac (0 :| []))
--
-- >>> testparsetheneof parseFrac "00"
-- Right (Frac (0 :| [0]))
--
-- >>> testparsetheneof parseFrac "01"
-- Right (Frac (0 :| [1]))
--
-- >>> testparsethennoteof parseFrac "01x"
-- Right (Frac (0 :| [1]))
parseFrac ::
  (Monad f, CharParsing f) =>
  f Frac
parseFrac =
  Frac <$> some1 parsedigit <?> "fractional digits"

-- |
--
-- >>> testparsethen parseExp "e10x"
-- Right (Exp {_ex = Ee, _minusplus = Nothing, _expdigits = 1 :| [0]},'x')
--
-- >>> testparsethen parseExp "e+10x"
-- Right (Exp {_ex = Ee, _minusplus = Just False, _expdigits = 1 :| [0]},'x')
--
-- >>> testparsethen parseExp "e-0x"
-- Right (Exp {_ex = Ee, _minusplus = Just True, _expdigits = 0 :| []},'x')
--
-- >>> testparsethen parseExp "E-1x"
-- Right (Exp {_ex = EE, _minusplus = Just True, _expdigits = 1 :| []},'x')
parseExp ::
  (Monad f, CharParsing f) =>
  f Exp
parseExp =
  Exp <$>
    parseE <*>
    optional (asum [False <$ char '+', True <$ char '-']) <*>
    (parsedigitlist1 <?> "exponent digits")

-- |
--
-- >>> testparsethen parseJNumber "3x"
-- Right (JNumber {_minus = False, _numberint = JIntInt D3_1to9 [], _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "-3x"
-- Right (JNumber {_minus = True, _numberint = JIntInt D3_1to9 [], _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "0x"
-- Right (JNumber {_minus = False, _numberint = JZero, _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "-0x"
-- Right (JNumber {_minus = True, _numberint = JZero, _frac = Nothing, _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "3.45x"
-- Right (JNumber {_minus = False, _numberint = JIntInt D3_1to9 [], _frac = Just (Frac (4 :| [5])), _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "-3.45x"
-- Right (JNumber {_minus = True, _numberint = JIntInt D3_1to9 [], _frac = Just (Frac (4 :| [5])), _expn = Nothing},'x')
--
-- >>> testparsethen parseJNumber "3.45e10x"
-- Right (JNumber {_minus = False, _numberint = JIntInt D3_1to9 [], _frac = Just (Frac (4 :| [5])), _expn = Just (Exp {_ex = Ee, _minusplus = Nothing, _expdigits = 1 :| [0]})},'x')
--
-- >>> testparsethen parseJNumber "3e10x"
-- Right (JNumber {_minus = False, _numberint = JIntInt D3_1to9 [], _frac = Nothing, _expn = Just (Exp {_ex = Ee, _minusplus = Nothing, _expdigits = 1 :| [0]})},'x')
--
-- >>> testparsethen parseJNumber "3.45e+10x"
-- Right (JNumber {_minus = False, _numberint = JIntInt D3_1to9 [], _frac = Just (Frac (4 :| [5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just False, _expdigits = 1 :| [0]})},'x')
--
-- >>> testparsethen parseJNumber "-3.45e-02x"
-- Right (JNumber {_minus = True, _numberint = JIntInt D3_1to9 [], _frac = Just (Frac (4 :| [5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just True, _expdigits = 0 :| [2]})},'x')
--
-- >>> isLeft (testparsethen parseJNumber "-3.45ex")
-- True
--
-- >>> isLeft (testparsethen parseJNumber "-.45e1x")
-- True
parseJNumber ::
  (Monad f, CharParsing f) =>
  f JNumber
parseJNumber =
  JNumber <$>
    isJust <$> optional (char '-') <*>
    (parseJInt <?> "number integral") <*>
    (optional (char '.' Applicative.*> parseFrac) <?> "fractional digits") <*>
    (optional parseExp <?> "exponent")


-- | Returns a normalised 'Scientific' value or Nothing if the exponent
--   is out of the range @[minBound,maxBound::Int]@
--
-- >>> jNumberToScientific JNumber {_minus = True, _numberint = JIntInt D3_1to9 [], _frac = Just (Frac (x4 :| [x5])), _expn = Just (Exp {_ex = Ee, _minusplus = Just True, _expdigits = x0 :| [x2]})}
-- Just -3.45e-2
--
-- >>> jNumberToScientific JNumber {_minus = True, _numberint = JIntInt D1_1to9 [x2,x3], _frac = Just (Frac (x4 :| [x5,x6])), _expn = Just (Exp {_ex = Ee, _minusplus = Just True, _expdigits = x7 :| [x8,x9]})}
-- Just -1.23456e-787
--
-- >>> jNumberToScientific JNumber {_minus = True, _numberint = JIntInt D1_1to9 [x2,x3], _frac = Just (Frac (x4 :| [x5,x6])), _expn = Just (Exp {_ex = Ee, _minusplus = Just False, _expdigits = x7 :| [x8,x9]})}
-- Just -1.23456e791
--

jNumberToScientific :: JNumber -> Maybe Scientific
jNumberToScientific (JNumber sign int mfrac mexp) =
    if nexp > fromIntegral (maxBound :: Int) ||
       nexp < fromIntegral (minBound :: Int)
         then Nothing
         else Just (scientific ncoeff (fromInteger nexp))
    where
        intDigs       = jIntToDigits int

        fracList      = mfrac ^.. _Just . _Wrapped . traverse
        exponentShift = Data.List.length fracList

        coeff         = neg (Just sign) (digitlist # intDigs ++ fracList)

        expon         = maybe 0 expval mexp - fromIntegral exponentShift

        (ncoeff,nexp) = normaliseExponent coeff expon

        neg (Just True) = negate
        neg _           = id

        expval (Exp _ msign digs) = neg msign (digitlist # toList digs)

        normaliseExponent :: Integer -> Integer -> (Integer,Integer)
        normaliseExponent coef !expo = case coef `divMod` 10 of
            (coef',0) -> normaliseExponent coef' (expo+1)
            _         -> (coef, expo)



jIntToInteger :: JInt -> Integer
jIntToInteger jsi =
    digitlist # jIntToDigits jsi


jIntToDigits :: JInt -> [Digit]
jIntToDigits JZero = [x0]
jIntToDigits (JIntInt d ds) = digit1to9toDigit # d : fmap (view hasdigit) ds

-- | Convert a 'JString' to a strict 'Text'
--
-- >>> jStringToText (JString [EscapedJChar Tab, UnescapedJChar (JCharUnescaped '1'), EscapedJChar (Hex (HexDigit4 D1 D2 D3 D4)), UnescapedJChar (JCharUnescaped '2')])
-- "\t1\4660\&2"
jStringToText :: JString -> Text
jStringToText (JString jcs) = Text.pack (jCharToChar <$> jcs)

jCharToChar :: JChar -> Char
jCharToChar (UnescapedJChar (JCharUnescaped c)) = c
jCharToChar (EscapedJChar jca) = case jca of
    QuotationMark  -> '"'
    ReverseSolidus -> '\\'
    Solidus        -> '/'
    Backspace      -> '\b'
    FormFeed       -> '\f'
    LineFeed       -> '\n'
    CarriageReturn -> '\r'
    Tab            -> '\t'
    Hex (HexDigit4 a b c d) ->
        chr (foldl (\acc x -> 16*acc + hexToInt x) 0 [a,b,c,d])

hexToInt :: HexDigit -> Int
hexToInt h = case h of
    D0 -> 0
    D1 -> 1
    D2 -> 2
    D3 -> 3
    D4 -> 4
    D5 -> 5
    D6 -> 6
    D7 -> 7
    D8 -> 8
    D9 -> 9
    Da -> 10
    Db -> 11
    Dc -> 12
    Dd -> 13
    De -> 14
    Df -> 15
    DA -> 10
    DB -> 11
    DC -> 12
    DD -> 13
    DE -> 14
    DF -> 15
