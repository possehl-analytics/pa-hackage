{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Json.Enc where

import Data.Aeson (Encoding, Value (..))
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as Aeson.Pretty
import Data.Aeson.Encoding qualified as AesonEnc
import Data.Aeson.Encoding qualified as Json.Enc
import Data.Aeson.Encoding qualified as Json.Encoding
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LazyBytes
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Scientific
import Data.String (IsString (fromString))
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Builder qualified as Text.Builder
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as ISO8601
import GHC.TypeLits
import PossehlAnalyticsPrelude

-- | A JSON encoder.
--
-- It is faster than going through 'Value', because 'Encoding' is just a wrapper around a @Bytes.Builder@.
-- But the @aeson@ interface for 'Encoding' is extremely bad, so let’s build a better one.
newtype Enc = Enc {unEnc :: Encoding}
  deriving (Num, Fractional) via (NumLiteralOnly "Enc" Enc)

instance Show Enc where
  show e = e.unEnc & Json.Encoding.encodingToLazyByteString & bytesToTextUtf8UnsafeLazy & show

-- | You can create an @Enc any@ that renders an 'Aeson.String' value with @OverloadedStrings@.
instance IsString Enc where
  fromString = Enc . AesonEnc.string

-- | You can create an @Enc any@ that renders an 'Aeson.Number' value with an integer literal.
instance IntegerLiteral Enc where
  integerLiteral = Enc . AesonEnc.integer

-- | You can create an @Enc any@ that renders an 'Aeson.Number' value with an floating point literal.
--
-- __ATTN__: Bear in mind that this will crash on repeating rationals, so only use for literals in code!
instance RationalLiteral Enc where
  rationalLiteral r = Enc $ AesonEnc.scientific (r & fromRational @Scientific)

-- | Convert an 'Enc' to a strict UTF8-bytestring which is valid JSON (minified).
encToBytesUtf8 :: Enc -> ByteString
encToBytesUtf8 enc = enc & encToBytesUtf8Lazy & toStrictBytes

-- | Convert an 'Enc' to a lazy UTF8-bytestring which is valid JSON (minified).
encToBytesUtf8Lazy :: Enc -> LazyBytes.ByteString
encToBytesUtf8Lazy enc = enc.unEnc & Json.Enc.encodingToLazyByteString

-- | Convert an 'Enc' to a strict Text which is valid JSON (prettyfied).
--
-- __ATTN__: will re-parse the json through `Value`, so only use for user-interactions like pretty-printing.
encToTextPretty :: Enc -> Text
encToTextPretty enc =
  enc
    & encToTextPrettyLazy
    & toStrict

-- | Convert an 'Enc' to a lazy Text which is valid JSON (prettyfied).
--
-- __ATTN__: will re-parse the json through `Value`, so only use for user-interactions like pretty-printing.
encToTextPrettyLazy :: Enc -> Lazy.Text
encToTextPrettyLazy enc =
  enc
    & encToBytesUtf8Lazy
    & Json.decode @Json.Value
    & annotate "the json parser can’t parse json encodings??"
    & unwrapError
    & Aeson.Pretty.encodePrettyToTextBuilder
    & Text.Builder.toLazyText

-- | Embed an 'Encoding' verbatim (it’s a valid JSON value)
encoding :: Encoding -> Enc
encoding = Enc

-- | Encode a 'Value' verbatim (it’s a valid JSON value)
value :: Value -> Enc
value = Enc . AesonEnc.value

-- | Encode an empty 'Array'
emptyArray :: Enc
emptyArray = Enc AesonEnc.emptyArray_

-- | Encode an empty 'Object'
emptyObject :: Enc
emptyObject = Enc AesonEnc.emptyObject_

-- | Encode a 'Text'
text :: Text -> Enc
text = Enc . AesonEnc.text

-- | Encode a lazy @Text@
lazyText :: Lazy.Text -> Enc
lazyText = Enc . AesonEnc.lazyText

-- | Encode a 'String'
string :: String -> Enc
string = Enc . AesonEnc.string

-- | Encode as 'Null' if 'Nothing', else use the given encoder for @Just a@
nullOr :: (a -> Enc) -> Maybe a -> Enc
nullOr inner = \case
  Nothing -> Enc AesonEnc.null_
  Just a -> inner a

-- | Encode a list as 'Array'
list :: (a -> Enc) -> [a] -> Enc
list f = Enc . AesonEnc.list (\a -> (f a).unEnc)

-- | Encode a 'NonEmpty' as an 'Array'.
nonEmpty :: (a -> Enc) -> NonEmpty a -> Enc
nonEmpty f = list f . toList

-- | Encode the given list of keys and their encoders as 'Object'.
--
-- Like with 'Map.fromList', if the list contains the same key multiple times, the last value in the list is retained:
--
-- @
-- (object [ ("foo", 42), ("foo", 23) ])
-- ~= "{\"foo\":23}"
-- @
object :: Foldable t => t (Text, Enc) -> Enc
object m =
  Enc $
    AesonEnc.dict
      AesonEnc.text
      (\recEnc -> recEnc.unEnc)
      Map.foldrWithKey
      (Map.fromList $ toList m)

-- | A tag/value encoder; See 'choice'
data Choice = Choice Text Enc

-- | Encode a sum type as a @Choice@, an object with a @tag@/@value@ pair,
-- which is the conventional json sum type representation in our codebase.
--
-- @
-- foo :: Maybe Text -> Enc
-- foo = choice $ \case
--   Nothing -> Choice "no" emptyObject ()
--   Just t -> Choice "yes" text t
--
-- ex = foo Nothing == "{\"tag\": \"no\", \"value\": {}}"
-- ex2 = foo (Just "hi") == "{\"tag\": \"yes\", \"value\": \"hi\"}"
-- @
choice :: (from -> Choice) -> from -> Enc
choice f from = case f from of
  Choice key encA -> singleChoice key encA

-- | Like 'choice', but simply encode a single possibility into a @tag/value@ object.
-- This can be useful, but if you want to match on an enum, use 'choice' instead.
singleChoice :: Text -> Enc -> Enc
singleChoice key encA =
  Enc $
    AesonEnc.pairs $
      mconcat
        [ AesonEnc.pair "tag" (AesonEnc.text key),
          AesonEnc.pair "value" encA.unEnc
        ]

-- | Encode a 'Map'.
--
-- We can’t really set the key to anything but text (We don’t keep the tag of 'Encoding')
-- so instead we allow anything that’s coercible from text as map key (i.e. newtypes).
map :: forall k v. (Coercible k Text) => (v -> Enc) -> Map k v -> Enc
map valEnc m =
  Enc $
    AesonEnc.dict
      (AesonEnc.text . coerce @k @Text)
      (\v -> (valEnc v).unEnc)
      Map.foldrWithKey
      m

-- | Encode a 'KeyMap'
keyMap :: (v -> Enc) -> KeyMap v -> Enc
keyMap valEnc m =
  Enc $
    AesonEnc.dict
      (AesonEnc.text . Key.toText)
      (\v -> (valEnc v).unEnc)
      KeyMap.foldrWithKey
      m

-- | Encode 'Null'
null :: Enc
null = Enc AesonEnc.null_

-- | Encode 'Bool'
bool :: Bool -> Enc
bool = Enc . AesonEnc.bool

-- | Encode an 'Integer' as 'Number'.
-- TODO: is it okay to just encode an arbitrarily-sized integer into json?
integer :: Integer -> Enc
integer = Enc . AesonEnc.integer

-- | Encode a 'Scientific' as 'Number'.
scientific :: Scientific -> Enc
scientific = Enc . AesonEnc.scientific

-- | Encode a 'Natural' as 'Number'.
natural :: Natural -> Enc
natural = integer . toInteger @Natural

-- | Encode an 'Int' as 'Aeson.Number'.
int :: Int -> Enc
int = Enc . AesonEnc.int

-- | Encode an 'Int64' as 'Number'.
int64 :: Int64 -> Enc
int64 = Enc . AesonEnc.int64

-- | Encode UTCTime as 'String', as an ISO8601 timestamp with timezone (@yyyy-mm-ddThh:mm:ss[.sss]Z@)
utcTime :: Time.UTCTime -> Enc
utcTime =
  text . stringToText . ISO8601.iso8601Show @Time.UTCTime

-- | Implement this class if you want your type to only implement the part of 'Num'
-- that allows creating them from Integer-literals, then derive Num via 'NumLiteralOnly':
--
-- @
-- data Foo = Foo Integer
--   deriving (Num) via (NumLiteralOnly "Foo" Foo)
--
-- instance IntegerLiteral Foo where
--  integerLiteral i = Foo i
-- @
class IntegerLiteral a where
  integerLiteral :: Integer -> a

-- | The same as 'IntegerLiteral' but for floating point literals.
class RationalLiteral a where
  rationalLiteral :: Rational -> a

-- | Helper class for @deriving (Num) via …@, implements only literal syntax for integer and floating point numbers,
-- and throws descriptive runtime errors for any other methods in 'Num'.
--
-- See 'IntegerLiteral' and 'RationalLiteral' for examples.
newtype NumLiteralOnly (sym :: Symbol) num = NumLiteralOnly num

instance (IntegerLiteral num, KnownSymbol sym) => Num (NumLiteralOnly sym num) where
  fromInteger = NumLiteralOnly . integerLiteral
  (+) = error [fmt|Only use as numeric literal allowed for {symbolVal (Proxy @sym)}, you tried to add (+) (NumLiteralOnly)|]
  (*) = error [fmt|Only use as numeric literal allowed for {symbolVal (Proxy @sym)}, you tried to multiply (*) (NumLiteralOnly)|]
  (-) = error [fmt|Only use as numeric literal allowed for {symbolVal (Proxy @sym)}, you tried to subtract (-) (NumLiteralOnly)|]
  abs = error [fmt|Only use as numeric literal allowed for {symbolVal (Proxy @sym)}, you tried to use `abs` (NumLiteralOnly)|]
  signum = error [fmt|Only use as numeric literal allowed for {symbolVal (Proxy @sym)}, you tried to use `signum` (NumLiteralOnly)|]

instance (IntegerLiteral num, RationalLiteral num, KnownSymbol sym) => Fractional (NumLiteralOnly sym num) where
  fromRational = NumLiteralOnly . rationalLiteral
  recip = error [fmt|Only use as rational literal allowed for {symbolVal (Proxy @sym)}, you tried to use `recip` (NumLiteralOnly)|]
  (/) = error [fmt|Only use as numeric literal allowed for {symbolVal (Proxy @sym)}, you tried to divide (/) (NumLiteralOnly)|]
