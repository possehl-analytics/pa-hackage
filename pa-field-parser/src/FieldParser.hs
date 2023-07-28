{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module FieldParser where

import Control.Category qualified as Cat
import Control.Monad ((<=<))
import Data.Aeson qualified as Json
import Data.Aeson.BetterErrors qualified as Json
import Data.Aeson.Types qualified as Json
import Data.Attoparsec.ByteString qualified as AttoBytes
import Data.Attoparsec.Text qualified as Atto
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.Error.Tree
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Scientific qualified as Scientific
import Data.Semigroup.Foldable (Foldable1 (toNonEmpty))
import Data.Semigroupoid qualified as Semigroupoid
import Data.Text qualified as Text
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as Time.Format.ISO
import PossehlAnalyticsPrelude
import Text.ParserCombinators.ReadPrec qualified as Read
import Prelude hiding (or)

-- | Parser for a field. TODO: define what a field is
--
-- If you want to build more complex parsers, use the 'attoparsecText' and 'attoparsecBytes' functions
-- to build a parser from a bytestring.
--
-- If you want to nest parsers, e.g. first want to decode to Text via utf-8 and then parse the Text,
-- use the 'Semigroupoid'/'Category' instances to chain parsers.
--
-- As a general rule, when you create an error message, try to include the value
-- (or a shortened version of the value) that was not accepted.
-- Otherwise the error will be hard to debug.

-- TODO: Can we add some examples to each parser?
newtype FieldParser' err from to = FieldParser (from -> Either err to)
  deriving stock (Functor)

-- | An alias for 'FieldParser'' for the common case where @err@ = 'Error'.
type FieldParser from to = FieldParser' Error from to

-- | If the right parser fails, return its error, otherwise run the left parser.
instance Semigroupoid (FieldParser' err) where
  o :: FieldParser' err middle to -> FieldParser' err from middle -> FieldParser' err from to
  o (FieldParser f) (FieldParser g) = FieldParser (f <=< g)

-- | `id` is the parser that always succeeds.
instance Cat.Category (FieldParser' err) where
  id :: FieldParser' err a a
  id = FieldParser pure
  (.) = Semigroupoid.o

-- | You can map over both sides of a parser to change the types in a
instance Profunctor (FieldParser' err) where
  dimap :: (from' -> from) -> (to -> to') -> FieldParser' err from to -> FieldParser' err from' to'
  dimap pre post (FieldParser parser) = FieldParser (fmap post . parser . pre)

-- | Execute the field parser.
runFieldParser :: FieldParser' err from to -> from -> Either err to
runFieldParser (FieldParser fn) = fn

-- | Change the type of the `err` in a field Parser.`
mapError :: (err1 -> err2) -> FieldParser' err1 from to -> FieldParser' err2 from to
mapError f (FieldParser original) = FieldParser $ \from -> original from & first f

-- | Turn a @FieldParser Value@ directly into a valid 'parseJSON` implementation.
--
-- If you want to parse any objects or lists, it’s better to use 'Json.toAesonParser' with 'jsonParser' instead, but for simple json scalars this one is better.
toParseJSON ::
  FieldParser Json.Value a ->
  Json.Value ->
  Json.Parser a
toParseJSON parser =
  Json.toAesonParser
    prettyError
    (jsonParser parser)

-- | Turn a @FieldParser' ErrorTree Value@ directly into a valid 'parseJSON` implementation.
--
-- If you want to parse any objects or lists, it’s better to use 'Json.toAesonParser' with 'jsonParser' instead, but for simple json scalars this one is better.
toParseJSONErrorTree ::
  FieldParser' ErrorTree Json.Value a ->
  Json.Value ->
  Json.Parser a
toParseJSONErrorTree parser =
  Json.toAesonParser
    prettyErrorTree
    (jsonParser parser)

toReadPrec ::
  -- | ReadPrec to base this parser on (e.g. use @readPrec \@Text@@ to parse the same as Text)
  Read.ReadPrec from ->
  FieldParser from to ->
  Read.ReadPrec to
toReadPrec innerReadPrec parser = do
  from :: from <- innerReadPrec
  case runFieldParser parser from of
    Left err -> fail (err & prettyError & textToString)
    Right a -> pure a

-- | Turn a @FieldParser Value@ into an 'Json.ParseT` which parses this value.
jsonParser :: Monad m => FieldParser' err Json.Value to -> Json.ParseT err m to
jsonParser parser =
  ( Json.asValue
      >>= ( \from ->
              runFieldParser parser from & \case
                Right a -> pure a
                Left err -> Json.throwCustomError err
          )
  )

-- TODO: provide nice shortened values in the error messages for these json parsers.

-- | parse a json boolean from a 'Json.Value'
jsonBool :: FieldParser Json.Value Bool
jsonBool = FieldParser $ \case
  Json.Bool b -> Right b
  _ -> Left "Not a json boolean"

-- | parse a json `null` from a 'Json.Value'
jsonNull :: FieldParser Json.Value ()
jsonNull = FieldParser $ \case
  Json.Null -> Right ()
  _ -> Left "Not a json `null`"

-- | parse a json number from a 'Json.Value'
jsonNumber :: FieldParser Json.Value Scientific
jsonNumber = FieldParser $ \case
  Json.Number s -> Right s
  _ -> Left "Not a json number"

-- | parse a json string from a 'Json.Value'
jsonString :: FieldParser Json.Value Text
jsonString = FieldParser $ \case
  Json.String s -> Right s
  _ -> Left "Not a json string"

-- * Field parsers

-- | Parse field as 'Text'
utf8 :: FieldParser ByteString Text
utf8 = FieldParser $ \bytes -> case bytesToTextUtf8 bytes of
  Left _err -> Left $ "Not a valid UTF-8 string"
  Right a -> Right a

-- | Assert that the string is not empty
notEmptyStringP :: FieldParser Text Text
notEmptyStringP = FieldParser $ \case
  "" -> Left [fmt|String cannot be empty|]
  t -> Right t

-- | A decimal number with an optional `+` or `-` sign character.
signedDecimal :: FieldParser Text Integer
signedDecimal =
  attoparsecText
    (\t -> [fmt|Not a signed decimal number: "{t}"|])
    -- the decimal is okay, since Integer has no maximum length
    -- we don’t have to care about memory, since the input text would already not fit into memory
    (Atto.signed (Atto.decimal @Integer))

-- | A decimal natural number; does not allow for a @+@-sign.
decimalNatural :: FieldParser Text Natural
decimalNatural =
  attoparsecText
    (\t -> [fmt|Not a natural number: "{t}"|])
    -- the decimal is okay, since Natural has no maximum length
    -- we don’t have to care about memory, since the input text would already not fit into memory
    (Atto.decimal @Integer)
    Cat.>>> integralToNatural @Integer

-- | A signed, decimal, natural number.
--
-- e.g. @12345@, @0@, or @+12@, but not @-12@.
signedDecimalNatural :: FieldParser Text Natural
signedDecimalNatural =
  attoparsecText
    (\t -> [fmt|Not a signed natural number: "{t}"|])
    -- the decimal is okay, since Natural has no maximum length
    -- we don’t have to care about memory, since the input text would already not fit into memory
    (Atto.signed (Atto.decimal @Integer))
    Cat.>>> integralToNatural @Integer

-- | Parse any integral into a natural number, fails if the integral is negative.
integralToNatural :: Integral i => FieldParser i Natural
integralToNatural =
  FieldParser (\i -> i & intToNatural & annotate [fmt|Number must be 0 or positive, but was negative: {toInteger i}|])

-- | Parse any integral to an 'Integer'. This can never fail, but is here to mirror 'integralToNatural'.
integralToInteger :: Integral i => FieldParser' err i Integer
integralToInteger = lmap (fromIntegral @_ @Integer) Cat.id

-- | An arbitrary-precision number in scientific notation.
scientific :: FieldParser Text Scientific
scientific = attoparsecText (\t -> [fmt|Not a scientific number: "{t}"|]) Atto.scientific

-- | Parse a scientific into a bounded integral type.
--
-- Scientific can be *very* big, (think @1e10000@) so this function makes sure we
-- * don’t wrap around the bound
-- * don’t fill up all our memory by e.g. parsing into @Integer@ or @Natural@.
--
-- So if you want to go to @Natural@, you have to first set an intermediate type with a bound you want to accept
-- (e.g. 64 bits via @Int@) and then go from that to the unbounded type (e.g. via 'integralToNatural').
--
-- @err@ is added as context around the bounded error.
boundedScientificIntegral :: forall i. (Integral i, Bounded i) => Error -> FieldParser Scientific i
boundedScientificIntegral err = FieldParser $ \s -> case Scientific.toBoundedInteger s of
  Nothing -> Left $ (err & errorContext [fmt|Must be between {iMinBound} and {iMaxBound}|])
  Just i -> Right i
  where
    iMinBound = toInteger (minBound :: i)
    iMaxBound = toInteger (maxBound :: i)

-- | Parse a scientific into a bounded floating point type.
--
-- Scientific can be *very* big, (think @1e10000@) so this function makes sure we
-- * don’t wrap around the bound
-- * don’t fill up all our memory
-- * Fit into the available floating point representation space
boundedScientificRealFloat :: RealFloat d => FieldParser Scientific d
boundedScientificRealFloat = FieldParser $ \s ->
  Scientific.toBoundedRealFloat s
    & first
      ( \zeroOrInf ->
          ( if
                | 0 == zeroOrInf -> [fmt|Number {show s} is too small to fit into floating point.|]
                | isInfinite zeroOrInf -> [fmt|Number {show s} is too big to fit into floating point.|]
                | otherwise -> [fmt|Number {show s} did not fit into floating point, but we don’t know why (BUG).|]
          )
      )

-- | Parse an integer into a bounded integral type.
--
-- @err@ is added as context around the bounded error.
bounded :: forall i. (Integral i, Bounded i) => Text -> FieldParser Integer i
bounded err = FieldParser $ \num -> case num & fromIntegerBounded of
  Nothing -> Left $ (errorContext err [fmt|Must be between {iMinBound} and {iMaxBound}, but was: {num & toInteger}|])
  Just i -> Right i
  where
    -- from Scientific.toBoundedInteger
    fromIntegerBounded :: Integer -> Maybe i
    fromIntegerBounded i
      | i < iMinBound || i > iMaxBound = Nothing
      | otherwise = Just $ fromInteger i
    iMinBound = toInteger (minBound :: i)
    iMaxBound = toInteger (maxBound :: i)

-- | ex: @2021-02-23@
hyphenatedDay :: FieldParser Text Time.Day
hyphenatedDay =
  FieldParser $ \t ->
    case parseDay t of
      Nothing -> Left $ [fmt|Not a valid date of format yyyy-mm-dd: "{t}"|]
      Just day -> Right day
  where
    parseDay :: Text -> Maybe Time.Day
    parseDay t =
      t
        & textToString
        & Time.Format.ISO.iso8601ParseM @Maybe @Time.Day

-- | @yyyy-mm-ddThh:mm:ss[.sss]Z@ (ISO 8601:2004(E) sec. 4.3.2 extended format)
utcTime :: FieldParser Text Time.UTCTime
utcTime =
  FieldParser $ \t ->
    case parseTime t of
      Nothing -> Left $ [fmt|Not a valid date of format `yyyy-mm-ddThh:mm:ss[.sss]Z` (ISO 8601:2004(E) sec. 4.3.2 extended format): "{t}"|]
      Just day -> Right day
  where
    parseTime :: Text -> Maybe Time.UTCTime
    parseTime t =
      t
        & textToString
        & Time.Format.ISO.iso8601ParseM @Maybe @Time.UTCTime

-- | Example of how to create a more “complicated” parser that checks whether a value
-- is between two other values.
clamped ::
  (Ord a, Show a) =>
  -- | lower boundary (inclusive)
  a ->
  -- | upper boundary (exclusive)
  a ->
  FieldParser a a
clamped lower upperExcl = FieldParser $ \a ->
  if a >= lower && a < upperExcl
    then Right a
    else Left $ [fmt|Value not between {lower & show} (inclusive) and {upperExcl & show} (exclusive), was: {show a}|]

-- | @oneOf prettyFrom oneOfMap@
--
-- Takes a @oneOf@, which is a list of possibilities that this parser accepts.
-- The comparison is done with '(==)', and then the according 'to' value is returned.
--
-- In case of an error @prettyFrom@ is used to pretty-print the available choices and actual input.
--
-- If you want to match on an 'Enum'-like type,
-- you should probably use 'invertPretty' or 'invertPrettyCaseInsensitive' instead,
-- which allows for exhaustiveness checks.
oneOf :: Ord from => (from -> Text) -> [(from, to)] -> FieldParser from to
oneOf errDisplay m =
  -- This doesn’t strictly need an 'Ord' instance, it can also use `findMaybe` with `==` instead of going through a map.
  oneOfMap errDisplay (Map.fromList m)

-- | 'oneOf', but takes a map directly.
--
-- | @oneOfMap prettyFrom oneOfMap@
--
-- Takes a @oneOfMap@, which is a map of possibilities that this parser accepts.
--
-- In case of an error @prettyFrom@ is used to pretty-print the available choices and actual input.
oneOfMap :: (Ord from) => (from -> Text) -> Map from to -> FieldParser from to
oneOfMap errDisplay m = FieldParser $ \from ->
  m
    & Map.lookup from
    & \case
      Nothing -> do
        let prettyFrom f = [fmt|"{f & errDisplay}"|]
        Left $ [fmt|Not one of: {m & Map.keys <&> prettyFrom & Text.intercalate ", "}, was {from & prettyFrom}|]
      Just to -> Right to

-- | Parse into an enum from a textual description of the fields.
--
-- The given function is inverted with 'inverseMap' and then used as the parsing function.
textEnum :: (Bounded to, Enum to) => (to -> Text) -> FieldParser Text to
textEnum displayEnum = oneOfMap id (inverseMap displayEnum)

-- | Try to run the first parser, or if it fails run the second one; return an Either.
either :: FieldParser from to1 -> FieldParser from to2 -> FieldParser' ErrorTree from (Either to1 to2)
either first' second' =
  FieldParser $ \from -> case runFieldParser first' from of
    Left err -> case runFieldParser second' from of
      Left err2 ->
        Left $ errorTree "Neither the left nor the right parser succeeded" $ err :| [err2]
      Right a -> Right $ Right a
    Right a -> Right $ Left a

-- | Try to run the first parser, or if it fails run the next one; They have to return the same value.
or :: NonEmpty (FieldParser from to) -> FieldParser' ErrorTree from to
or parsers =
  FieldParser $ \from ->
    parsers
      & toNonEmpty
      & traverse
        ( \p ->
            runFieldParser p from
              -- we want to shortcut on the first successful parser, using Left
              & flipEither
        )
      & \case
        Left a -> Right $ a
        Right errs -> Left $ errorTree "Neither of these parsers succeeded" errs
  where
    flipEither :: Either a b -> Either b a
    flipEither (Left err) = Right err
    flipEither (Right a) = Left a

-- | Parse into Nothing if the Monoid (e.g. Text, Map etc.) was empty
emptyOr :: forall s a. (Eq s, Show s, Monoid s) => FieldParser s a -> FieldParser' Error s (Maybe a)
emptyOr inner =
  FieldParser $ \from ->
    if from == mempty
      then Right Nothing
      else case runFieldParser inner from of
        Left err -> Left $ errorContext [fmt|Value was neither empty ("{mempty @s & show}") nor|] err
        Right a -> Right (Just a)

-- | Given a pretty printing function, it will create a parser
-- that uses the inverse function to parse the field.
--
-- The pretty printing function must create a different output for different inputs!
-- Otherwise which value is returned is undefined.
invertPretty :: (Bounded to, Enum to) => (to -> Text) -> FieldParser Text to
invertPretty prettyFn = oneOfMap id (inverseMap prettyFn)

-- | Given a pretty printing function, it will create a parser
-- that uses the inverse function to parse the field.
-- The parsed text is compared case-insensitively.
--
-- The pretty printing function must create a different output for different inputs!
-- This also means two outputs should not match if compared case-insensitively.
-- Otherwise which value is returned is undefined.
invertPrettyCaseInsensitive :: (Bounded to, Enum to) => (to -> Text) -> FieldParser Text to
invertPrettyCaseInsensitive prettyFn =
  oneOfMap
    CaseInsensitive.original
    (inverseMap (\t -> prettyFn t & CaseInsensitive.mk))
    & lmap CaseInsensitive.mk

-- | 'oneOf' but only one value possible
exactly :: Eq from => (from -> Text) -> from -> FieldParser from from
exactly errDisplay from = FieldParser $ \from' ->
  if from == from'
    then Right from'
    else Left $ [fmt|Field has to be exactly {errDisplay from}, was: {errDisplay from'}|]

-- | Takes a parser and lifts it to parse every element in a list.
multiple ::
  -- \| @toLevelError@: A descriptive message about the context of how these multiple elements should be parsed
  --
  -- e.g. @"Must be a |-separated list of <foo> (e.g. <foo>|<foo>), but some elements could not be parsed"@.
  -- It is used as the root of the 'ErrorTree'.
  Text ->
  -- | For each sub-parser that failed, @displayValOnErr@ is prefixed to its error.
  -- It receives the index (starting from 1) of the element that failed,
  -- so you can display it in the element’s error message.
  -- You can decide yourself whether you want to print the full value, part of the value, or only the index.
  (Natural -> from -> Text) ->
  -- | Parser for each element
  FieldParser from to ->
  FieldParser' ErrorTree [from] [to]
multiple topLevelErr displayValOnErr inner = FieldParser $ \ta ->
  ta & toList & indexed & traverse run & \case
    Success b -> Right b
    Failure errs -> Left $ errorTree (newError topLevelErr) errs
  where
    indexed = zip [1 :: Natural ..]
    run (index, a) = case runFieldParser inner a of
      -- TODO: It would probably be nice to display the actual value that could not be parsed here!
      Left err -> Failure (singleton $ errorContext (displayValOnErr index a) err)
      Right res -> Success res

nonEmpty :: err -> FieldParser' err [from] (NonEmpty from)
nonEmpty msg = FieldParser $ \from -> do
  case from & NonEmpty.nonEmpty of
    Nothing -> Left msg
    Just ne -> Right ne

-- | Wrap a FieldParser with some descriptions for generating better error messages.
data FieldParserDesc' err from to = FieldParserDesc
  { -- | Symbolic description of a parser, e.g. “hh:mm” for a timestamp
    symbolicDesc :: Text,
    -- | Actual parser
    fieldParser :: FieldParser' err from to
  }

type FieldParserDesc from to = FieldParserDesc' Error from to

-- | Splits the input string into multiple elements based on the given separator string.
--
-- Each element is then passed to the provided @innerParser@.
--
-- This returns a descriptive tree of errors containing the errors of each sub-parser that failed.
separatedBy ::
  -- | Separator
  Text ->
  -- | For each sub-parser that failed, @displayValOnErr@ is prefixed to its error.
  -- It receives the index (starting from 1) of the element that failed,
  -- so you can display it in the element’s error message.
  -- You can decide yourself whether you want to print the full value, part of the value, or only the index.
  (Natural -> Text -> Text) ->
  FieldParserDesc Text to ->
  FieldParser' ErrorTree Text [to]
separatedBy separator displayValOnErr innerParser =
  ( Cat.id
      & rmap (Text.splitOn separator)
      & rmap
        ( \case
            -- splitOn for "" results in [""] but we want to accept a empty list
            [""] -> []
            xs -> xs
        )
      & mapError singleError
  )
    Cat.>>> ( multiple
                ( let d = innerParser.symbolicDesc
                   in [fmt|Must be a {separator}-separated list of {d} (e.g. "{d}{separator}{d}"), but some elements could not be parsed|]
                )
                displayValOnErr
                innerParser.fieldParser
            )

-- | Ignore whitespace around a text.
--
-- Shows how to use the profunctor instance to do pure transformations (that cannot throw any errors).
--
-- Alternatively this could be implemented in the FieldParser pipeline like
-- @
-- ignore = FieldParser $ \t -> Right (Text.strip t)
-- @
ignoreSurroundingWhitespace :: FieldParser Text a -> FieldParser Text a
ignoreSurroundingWhitespace = lmap Text.strip

-- | Given an error message and an attoparsec parser,
-- “clamp” the parser that it expects to match until the end of the string,
-- then run the parser and put the given error message on error.
--
-- This function works on "Data.Attoparsec.Text" parsers.
attoparsecText ::
  -- | Error message to use if the parser fails (the attoparsec message is discarded)
  (Text -> Error) ->
  -- | Parser to use. Should not check for `endOfInput` itself.
  Atto.Parser a ->
  FieldParser Text a
attoparsecText err parser =
  let parseAll = Atto.parseOnly (parser <* Atto.endOfInput)
   in FieldParser $ \text -> case parseAll text of
        Left _attoErr -> Left (err text)
        Right a -> Right a

-- | Given an error message and an attoparsec parser,
-- “clamp” the parser that it expects to match until the end of the string,
-- then run the parser and put the given error message on error.
--
-- This function works on "Data.Attoparsec.ByteString" parsers.
attoparsecBytes ::
  -- | Error message to use if the parser fails (the attoparsec message is discarded)
  Error ->
  -- | Parser to use. Should not check for `endOfInput` itself.
  AttoBytes.Parser a ->
  FieldParser ByteString a
attoparsecBytes err parser =
  let parseAll = AttoBytes.parseOnly (parser <* AttoBytes.endOfInput)
   in FieldParser $ \bytes -> case parseAll bytes of
        Left _attoErr -> Left err
        Right a -> Right a
