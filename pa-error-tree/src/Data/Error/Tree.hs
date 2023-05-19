module Data.Error.Tree where

import Data.String (IsString (..))
import Data.Tree qualified as Tree
import PossehlAnalyticsPrelude

-- | A tree of 'Error's, with a single root 'Error' and 0..n nested 'ErrorTree's.
--
-- @
-- top error
-- |
-- |-- error 1
-- | |
-- |  -- error 1.1
-- |
-- |-- error 2
-- @
newtype ErrorTree = ErrorTree {unErrorTree :: (Tree.Tree Error)}
  deriving stock (Show)

instance IsString ErrorTree where
  fromString = singleError . fromString

-- deriving newtype (Ord) -- TODO: Add this instance with containers-0.6.5

-- | Turn a single 'Error' into an 'ErrorTree', a leaf.
singleError :: Error -> ErrorTree
singleError e = ErrorTree $ Tree.Node e []

-- | Take a list of errors & create a new 'ErrorTree' with the given 'Error' as the root.
errorTree :: Error -> NonEmpty Error -> ErrorTree
errorTree topLevelErr nestedErrs =
  ErrorTree
    ( Tree.Node
        topLevelErr
        (nestedErrs <&> (\e -> Tree.Node e []) & toList)
    )

-- | Attach more context to the root 'Error' of the 'ErrorTree', via 'errorContext'.
errorTreeContext :: Text -> ErrorTree -> ErrorTree
errorTreeContext context (ErrorTree tree) =
  ErrorTree $
    tree
      { Tree.rootLabel = tree.rootLabel & errorContext context
      }

-- | Nest the given 'Error' around the ErrorTree
--
-- @
-- top level error
-- |
-- -- nestedError
--   |
--   -- error 1
--   |
--   -- error 2
-- @
nestedError ::
  Error -> -- top level
  ErrorTree -> -- nested
  ErrorTree
nestedError topLevelErr nestedErr =
  ErrorTree $
    Tree.Node
      { Tree.rootLabel = topLevelErr,
        Tree.subForest = [nestedErr.unErrorTree]
      }

-- | Nest the given 'Error' around the list of 'ErrorTree's.
--
-- @
-- top level error
-- |
-- |- nestedError1
-- | |
-- | -- error 1
-- | |
-- | -- error 2
-- |
-- |- nestedError 2
-- @
nestedMultiError ::
  Error -> -- top level
  NonEmpty ErrorTree -> -- nested
  ErrorTree
nestedMultiError topLevelErr nestedErrs =
  ErrorTree $
    Tree.Node
      { Tree.rootLabel = topLevelErr,
        Tree.subForest = nestedErrs & toList <&> (.unErrorTree)
      }

prettyErrorTree :: ErrorTree -> Text
prettyErrorTree (ErrorTree tree) =
  tree
    <&> prettyError
    <&> textToString
    & Tree.drawTree
    & stringToText

prettyErrorTrees :: NonEmpty ErrorTree -> Text
prettyErrorTrees forest =
  forest
    <&> (.unErrorTree)
    <&> fmap prettyError
    <&> fmap textToString
    & toList
    & Tree.drawForest
    & stringToText
