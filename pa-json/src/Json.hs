{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Json where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), Value (..), withObject)
import Data.Aeson.BetterErrors qualified as Json
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Error.Tree
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Label
import PossehlAnalyticsPrelude
import Test.Hspec.Core.Spec (describe, it)
import Test.Hspec.Core.Spec qualified as Hspec
import Test.Hspec.Expectations (shouldBe)

-- | Convert a 'Json.ParseError' to a corresponding 'ErrorTree'
--
-- TODO: build a different version of 'Json.displayError' so that we can nest 'ErrorTree' as well
parseErrorTree :: Error -> Json.ParseError ErrorTree -> ErrorTree
parseErrorTree contextMsg errs =
  errs
    & Json.displayError prettyErrorTree
    & Text.intercalate "\n"
    & newError
    -- We nest this here because the json errors is multiline, so the result looks like
    --
    -- @
    -- contextMsg
    -- \|
    -- `- At the path: ["foo"]["bar"]
    --   Type mismatch:
    --   Expected a value of type object
    --   Got: true
    -- @
    & singleError
    & nestedError contextMsg

-- | Parse a key from the object, à la 'Json.key', return a labelled value.
--
-- We don’t provide a version that infers the json object key,
-- since that conflates internal naming with the external API, which is dangerous.
--
-- @@
-- do
--   txt <- keyLabel @"myLabel" "jsonKeyName" Json.asText
--   pure (txt :: Label "myLabel" Text)
-- @@
keyLabel ::
  forall label err m a.
  Monad m =>
  Text ->
  Json.ParseT err m a ->
  Json.ParseT err m (Label label a)
keyLabel = do
  keyLabel' (Proxy @label)

-- | Parse a key from the object, à la 'Json.key', return a labelled value.
-- Version of 'keyLabel' that requires a proxy.
--
-- @@
-- do
--   txt <- keyLabel' (Proxy @"myLabel") "jsonKeyName" Json.asText
--   pure (txt :: Label "myLabel" Text)
-- @@
keyLabel' ::
  forall label err m a.
  Monad m =>
  Proxy label ->
  Text ->
  Json.ParseT err m a ->
  Json.ParseT err m (Label label a)
keyLabel' Proxy key parser = label @label <$> Json.key key parser

-- | Parse an optional key from the object, à la 'Json.keyMay', return a labelled value.
--
-- We don’t provide a version that infers the json object key,
-- since that conflates internal naming with the external API, which is dangerous.
--
-- @@
-- do
--   txt <- keyLabelMay @"myLabel" "jsonKeyName" Json.asText
--   pure (txt :: Label "myLabel" (Maybe Text))
-- @@
keyLabelMay ::
  forall label err m a.
  Monad m =>
  Text ->
  Json.ParseT err m a ->
  Json.ParseT err m (Label label (Maybe a))
keyLabelMay = do
  keyLabelMay' (Proxy @label)

-- | Parse an optional key from the object, à la 'Json.keyMay', return a labelled value.
-- Version of 'keyLabelMay' that requires a proxy.
--
-- @@
-- do
--   txt <- keyLabelMay' (Proxy @"myLabel") "jsonKeyName" Json.asText
--   pure (txt :: Label "myLabel" (Maybe Text))
-- @@
keyLabelMay' ::
  forall label err m a.
  Monad m =>
  Proxy label ->
  Text ->
  Json.ParseT err m a ->
  Json.ParseT err m (Label label (Maybe a))
keyLabelMay' Proxy key parser = label @label <$> Json.keyMay key parser

-- | Like 'Json.key', but allows a list of keys that are tried in order.
--
-- This is intended for renaming keys in an object.
-- The first key is the most up-to-date version of a key, the others are for backward-compatibility.
--
-- If a key (new or old) exists, the inner parser will always be executed for that key.
keyRenamed :: Monad m => NonEmpty Text -> Json.ParseT err m a -> Json.ParseT err m a
keyRenamed (newKey :| oldKeys) inner =
  keyRenamedTryOldKeys oldKeys inner >>= \case
    Nothing -> Json.key newKey inner
    Just parse -> parse

-- | Like 'Json.keyMay', but allows a list of keys that are tried in order.
--
-- This is intended for renaming keys in an object.
-- The first key is the most up-to-date version of a key, the others are for backward-compatibility.
--
-- If a key (new or old) exists, the inner parser will always be executed for that key.
keyRenamedMay :: Monad m => NonEmpty Text -> Json.ParseT err m a -> Json.ParseT err m (Maybe a)
keyRenamedMay (newKey :| oldKeys) inner =
  keyRenamedTryOldKeys oldKeys inner >>= \case
    Nothing -> Json.keyMay newKey inner
    Just parse -> Just <$> parse

-- | Helper function for 'keyRenamed' and 'keyRenamedMay' that returns the parser for the first old key that exists, if any.
keyRenamedTryOldKeys :: Monad m => [Text] -> Json.ParseT err m a -> Json.ParseT err m (Maybe (Json.ParseT err m a))
keyRenamedTryOldKeys oldKeys inner = do
  oldKeys & traverse tryOld <&> catMaybes <&> nonEmpty <&> \case
    Nothing -> Nothing
    Just (old :| _moreOld) -> Just old
  where
    tryOld key =
      Json.keyMay key (pure ()) <&> \case
        Just () -> Just $ Json.key key inner
        Nothing -> Nothing

test_keyRenamed :: Hspec.Spec
test_keyRenamed = do
  describe "keyRenamed" $ do
    let parser = keyRenamed ("new" :| ["old"]) Json.asText
    let p = Json.parseValue @() parser
    it "accepts the new key and the old key" $ do
      p (Object (KeyMap.singleton "new" (String "text")))
        `shouldBe` (Right "text")
      p (Object (KeyMap.singleton "old" (String "text")))
        `shouldBe` (Right "text")
    it "fails with the old key in the error if the inner parser is wrong" $ do
      p (Object (KeyMap.singleton "old" Null))
        `shouldBe` (Left (Json.BadSchema [Json.ObjectKey "old"] (Json.WrongType Json.TyString Null)))
    it "fails with the new key in the error if the inner parser is wrong" $ do
      p (Object (KeyMap.singleton "new" Null))
        `shouldBe` (Left (Json.BadSchema [Json.ObjectKey "new"] (Json.WrongType Json.TyString Null)))
    it "fails if the key is missing" $ do
      p (Object KeyMap.empty)
        `shouldBe` (Left (Json.BadSchema [] (Json.KeyMissing "new")))
  describe "keyRenamedMay" $ do
    let parser = keyRenamedMay ("new" :| ["old"]) Json.asText
    let p = Json.parseValue @() parser
    it "accepts the new key and the old key" $ do
      p (Object (KeyMap.singleton "new" (String "text")))
        `shouldBe` (Right (Just "text"))
      p (Object (KeyMap.singleton "old" (String "text")))
        `shouldBe` (Right (Just "text"))
    it "allows the old and new key to be missing" $ do
      p (Object KeyMap.empty)
        `shouldBe` (Right Nothing)

-- | A simple type isomorphic to `()` that that transforms to an empty json object and parses
data EmptyObject = EmptyObject
  deriving stock (Show, Eq)

instance FromJSON EmptyObject where
  -- allow any fields, as long as its an object
  parseJSON = withObject "EmptyObject" (\_ -> pure EmptyObject)

instance ToJSON EmptyObject where
  toJSON EmptyObject = Object mempty
  toEncoding EmptyObject = toEncoding $ Object mempty

-- | Create a json array from a list of json values.
jsonArray :: [Value] -> Value
jsonArray xs = xs & Vector.fromList & Array
