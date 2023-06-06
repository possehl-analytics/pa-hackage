module Pretty
  ( -- * Pretty printing for error messages
    Err,
    showPretty,
    showPrettyJson,
    showPrettyJsonEncoding,
    printPretty,
    -- constructors hidden
    prettyErrs,
    message,
    messageString,
    pretty,
    prettyString,
    hscolour',
  )
where

import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as Aeson.Pretty
import Data.Aeson.Encoding qualified as Json.Enc
import Data.List qualified as List
import Data.Text.Lazy.Builder qualified as Text.Builder
import Language.Haskell.HsColour
  ( Output (TTYg),
    hscolour,
  )
import Language.Haskell.HsColour.ANSI (TerminalType (..))
import Language.Haskell.HsColour.Colourise
  ( defaultColourPrefs,
  )
import PossehlAnalyticsPrelude
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types
  ( Color (Red),
    ColorIntensity (Dull),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
  )
import Text.Nicify (nicify)

-- | Print any 'Show'able type to stderr, formatted nicely and in color. Very helpful for debugging.
printPretty :: Show a => a -> IO ()
printPretty a =
  a & showPretty & putStderrLn

showPretty :: Show a => a -> Text
showPretty a = a & pretty & (: []) & prettyErrs & stringToText

showPrettyJson :: Json.Value -> Text
showPrettyJson val =
  val
    & Aeson.Pretty.encodePrettyToTextBuilder
    & Text.Builder.toLazyText
    & toStrict

showPrettyJsonEncoding :: Json.Encoding -> Text
showPrettyJsonEncoding enc =
  -- We have to roundtrip through Value again
  enc
    & Json.Enc.encodingToLazyByteString
    & Json.decode @Json.Value
    & annotate "the json parser can’t parse json encodings??"
    & unwrapError
    & showPrettyJson

-- | Display a list of 'Err's as a colored error message
-- and abort the test.
prettyErrs :: [Err] -> String
prettyErrs errs = res
  where
    res = List.intercalate "\n" $ map one errs
    one = \case
      ErrMsg s -> color Red s
      ErrPrettyString s -> prettyShowString s
    -- Pretty print a String that was produced by 'show'
    prettyShowString :: String -> String
    prettyShowString = hscolour' . nicify

-- | Small DSL for pretty-printing errors
data Err
  = -- | Message to display in the error
    ErrMsg String
  | -- | Pretty print a String that was produced by 'show'
    ErrPrettyString String

-- | Plain message to display, as 'Text'
message :: Text -> Err
message = ErrMsg . textToString

-- | Plain message to display, as 'String'
messageString :: String -> Err
messageString = ErrMsg

-- | Any 'Show'able to pretty print
pretty :: Show a => a -> Err
pretty x = ErrPrettyString $ show x

-- | Pretty print a String that was produced by 'show'
prettyString :: String -> Err
prettyString s = ErrPrettyString s

-- Prettifying Helpers, mostly stolen from
-- https://hackage.haskell.org/package/hspec-expectations-pretty-diff-0.7.2.5/docs/src/Test.Hspec.Expectations.Pretty.html#prettyColor

hscolour' :: String -> String
hscolour' =
  hscolour (TTYg Ansi16Colour) defaultColourPrefs False False "" False

color :: Color -> String -> String
color c s = setSGRCode [SetColor Foreground Dull c] ++ s ++ setSGRCode [Reset]
