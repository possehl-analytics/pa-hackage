{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Label
  ( -- * Labels
    Label,
    label,
    label',
    getLabel,
    mapLabel,
    traverseLabel,

    -- * Named Tuples
    T2 (..),
    focusOnField,
    monoMapT2,
    tupleToT2,
    zipT2,
    unzipT2,
    T3 (..),
    monoMapT3,
    tupleToT3,
    zipT3,
    unzipT3,

    -- * Named Sums/Enums
    E2 (..),
    mapE2,
    monoMapE2,
    monoFoldE2,
    monoTraverseE2,
    partitionE2,
    isE21,
    isE22,
    getE21,
    getE22,
    E3 (..),
    mapE3,
  )
where

import Data.Data (Proxy (..))
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Typeable (Typeable)
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | A labelled value.
--
-- Use 'label'/'label'' to construct,
-- then use dot-syntax to get the inner value.
newtype Label (label :: Symbol) value = Label value
  deriving stock (Eq, Ord)
  deriving newtype (Typeable, Semigroup, Monoid)

instance (KnownSymbol label, Show value) => Show (Label label value) where
  showsPrec d (Label val) =
    showParen (d > 10) $
      showString "label @"
        . showsPrec 11 (symbolVal (Proxy @label))
        . showString " "
        . showsPrec 11 val

-- | Attach a label to a value; should be used with a type application to name the label.
--
-- @
-- let f = label @"foo" 'f' :: Label "foo" Char
-- in f.foo :: Char
-- @
--
-- Use dot-syntax to get the labelled value.
label :: forall label value. value -> Label label value
label value = Label value

-- | Attach a label to a value; Pass it a proxy with the label name in the argument type.
-- This is intended for passing through the label value;
-- you can also use 'label'.
--
--
-- @
-- let f = label' (Proxy @"foo") 'f' :: Label "foo" Char
-- in f.foo :: Char
-- @
--
-- Use dot-syntax to get the labelled value.
label' :: forall label value. (Proxy label) -> value -> Label label value
label' Proxy value = Label value

-- | Fetches the labelled value.
instance HasField label (Label label value) value where
  getField :: (Label label value) -> value
  getField (Label value) = value

-- | Fetch a value from a record, like 'getField', but also keep it wrapped by its label.
getLabel :: forall label record a. (HasField label record a) => record -> Label label a
getLabel rec = rec & getField @label & label @label

-- | 'fmap' over the contents of the labbelled value. Helper.
mapLabel :: forall label a b. (a -> b) -> Label label a -> Label label b
mapLabel f (Label a) = Label @label $ f a

-- | 'traverse' over the contents of the labbelled value. Helper.
traverseLabel :: forall label f a b. (Functor f) => (a -> f b) -> Label label a -> f (Label label b)
traverseLabel fab (Label a) = Label @label <$> fab a

-- | A named 2-element tuple. Since the elements are named, you can access them with `.`.
--
-- @
-- let t2 = T2 (label @"myfield" 'c') (label @"otherfield" True) :: T2 "myfield" Char "otherfield" Bool
-- in (
--   t2.myfield :: Char,
--   t2.otherfield :: Bool
-- )
-- @
data T2 (l1 :: Symbol) t1 (l2 :: Symbol) t2 = T2 (Label l1 t1) (Label l2 t2)
  deriving stock (Show, Eq, Ord)

-- | Access the first field by label
instance HasField l1 (T2 l1 t1 l2 t2) t1 where
  getField (T2 t1 _) = getField @l1 t1

-- | Access the second field by label
instance HasField l2 (T2 l1 t1 l2 t2) t2 where
  getField (T2 _ t2) = getField @l2 t2

instance (Semigroup t1, Semigroup t2) => Semigroup (T2 l1 t1 l2 t2) where
  T2 t1 t2 <> T2 t1' t2' = T2 (t1 <> t1') (t2 <> t2')

instance (Monoid t1, Monoid t2) => Monoid (T2 l1 t1 l2 t2) where
  mempty = T2 mempty mempty

-- | Given a record with some field, “focus” on that field by pulling it into the first part of the T2,
-- and put the original record into the second part of the T2.
--
-- This can be useful when you have a function that requires something with a field,
-- but the field itself is nested somewhere in the record.
--
-- Example:
--
-- @
-- data Foo = Foo
--   { nested :: Label "myId" Text
--   }
--
-- foo = Foo {nested = "hi"}
--
-- fn :: HasField "myId" rec Text => rec -> Text
-- fn rec = rec.myId <> "!"
--
-- x = fn (focusOnField @"myId" (.nested) foo) == "hi!"
-- @
--
-- Note that you will have to give `focusOnField` a type annotation of which label to use,
-- otherwise it cannot infer it.
focusOnField ::
  forall field rec subrec t.
  (HasField field subrec t) =>
  (rec -> subrec) ->
  rec ->
  T2 field t "dat" rec
focusOnField zoom rec = T2 (getLabel @field (rec & zoom)) (label @"dat" rec)

-- | Map a function over all fields in the tuple. All fields have to have the same type.
monoMapT2 :: (t -> t') -> T2 l1 t l2 t -> T2 l1 t' l2 t'
monoMapT2 f (T2 a b) = T2 (mapLabel f a) (mapLabel f b)

-- | Convert a tuple to a T2 by giving its elements names.
--
-- @tupleToT2 @"left" @"right" ('c', True) :: T2 "left" Char "right" Bool@
tupleToT2 :: forall l1 l2 t1 t2. (t1, t2) -> T2 l1 t1 l2 t2
tupleToT2 (t1, t2) = T2 (label @l1 t1) (label @l2 t2)

-- | If you have a tuple of lists, make it into a list of tuples. The names are attached to each value.
zipT2 ::
  forall l1 l2 t1 t2.
  ( HasField l1 (T2 l1 [t1] l2 [t2]) [t1],
    HasField l2 (T2 l1 [t1] l2 [t2]) [t2]
  ) =>
  T2 l1 [t1] l2 [t2] ->
  [T2 l1 t1 l2 t2]
zipT2 xs =
  zipWith
    (\t1 t2 -> T2 (label @l1 t1) (label @l2 t2))
    (getField @l1 xs)
    (getField @l2 xs)

-- | If you have a list of tuples, make it into a tuple of lists. The names are attached to each value.
unzipT2 :: forall l1 t1 l2 t2. [T2 l1 t1 l2 t2] -> T2 l1 [t1] l2 [t2]
unzipT2 xs = xs <&> toTup & unzip & fromTup
  where
    toTup :: forall a b. T2 a t1 b t2 -> (t1, t2)
    toTup (T2 a b) = (getField @a a, getField @b b)
    fromTup :: (a, b) -> T2 l1 a l2 b
    fromTup (t1, t2) = T2 (label @l1 t1) (label @l2 t2)

-- | A named 3-element tuple. Since the elements are named, you can access them with `.`. See 'T2' for an example.
data T3 (l1 :: Symbol) t1 (l2 :: Symbol) t2 (l3 :: Symbol) t3 = T3 (Label l1 t1) (Label l2 t2) (Label l3 t3)
  deriving stock (Show, Eq, Ord)

-- | Access the first field by label
instance HasField l1 (T3 l1 t1 l2 t2 l3 t3) t1 where
  getField (T3 t1 _ _) = getField @l1 t1

-- | Access the second field by label
instance HasField l2 (T3 l1 t1 l2 t2 l3 t3) t2 where
  getField (T3 _ t2 _) = getField @l2 t2

-- | Access the third field by label
instance HasField l3 (T3 l1 t1 l2 t2 l3 t3) t3 where
  getField (T3 _ _ t3) = getField @l3 t3

instance (Semigroup t1, Semigroup t2, Semigroup t3) => Semigroup (T3 l1 t1 l2 t2 l3 t3) where
  T3 t1 t2 t3 <> T3 t1' t2' t3' = T3 (t1 <> t1') (t2 <> t2') (t3 <> t3')

instance (Monoid t1, Monoid t2, Monoid t3) => Monoid (T3 l1 t1 l2 t2 l3 t3) where
  mempty = T3 mempty mempty mempty

-- | Map a function over all fields in the tuple. All fields have to have the same type.
monoMapT3 :: (t -> t') -> T3 l1 t l2 t l3 t -> T3 l1 t' l2 t' l3 t'
monoMapT3 f (T3 a b c) = T3 (mapLabel f a) (mapLabel f b) (mapLabel f c)

-- | Convert a tuple to a T3 by giving its elements names.
--
-- @tupleToT3 @"left" @"right" @"grip" ('c', True, Maybe 'x') :: T3 "left" Char "right" Bool "grip" (Maybe Char)@
tupleToT3 :: forall l1 l2 l3 t1 t2 t3. (t1, t2, t3) -> T3 l1 t1 l2 t2 l3 t3
tupleToT3 (t1, t2, t3) = T3 (label @l1 t1) (label @l2 t2) (label @l3 t3)

-- | If you have a tuple of lists, make it into a list of tuples. The names are attached to each value.
zipT3 ::
  forall l1 l2 t1 t2 l3 t3.
  ( HasField l1 (T3 l1 [t1] l2 [t2] l3 [t3]) [t1],
    HasField l2 (T3 l1 [t1] l2 [t2] l3 [t3]) [t2],
    HasField l3 (T3 l1 [t1] l2 [t2] l3 [t3]) [t3]
  ) =>
  T3 l1 [t1] l2 [t2] l3 [t3] ->
  [T3 l1 t1 l2 t2 l3 t3]
zipT3 xs =
  zipWith3
    (\t1 t2 t3 -> T3 (label @l1 t1) (label @l2 t2) (label @l3 t3))
    (getField @l1 xs)
    (getField @l2 xs)
    (getField @l3 xs)

-- | If you have a list of tuples, make it into a tuple of lists. The names are attached to each value.
unzipT3 :: forall l1 t1 l2 t2 l3 t3. [T3 l1 t1 l2 t2 l3 t3] -> T3 l1 [t1] l2 [t2] l3 [t3]
unzipT3 xs = xs <&> toTup & unzip3 & fromTup
  where
    toTup :: forall a b c. T3 a t1 b t2 c t3 -> (t1, t2, t3)
    toTup (T3 a b c) = (getField @a a, getField @b b, getField @c c)
    fromTup :: (a, b, c) -> T3 l1 a l2 b l3 c
    fromTup (t1, t2, t3) = T3 (label @l1 t1) (label @l2 t2) (label @l3 t3)

-- | A named 2-alternative sum (“'Either' with labels”).
data E2 (l1 :: Symbol) t1 (l2 :: Symbol) t2
  = E21 (Label l1 t1)
  | E22 (Label l2 t2)
  deriving stock (Eq, Show)

instance (Bounded t1, Bounded t2) => Bounded (E2 l1 t1 l2 t2) where
  minBound = E21 (label @l1 minBound)
  maxBound = E22 (label @l2 maxBound)

-- TODO: instance for arbitrary Enum types?
instance Enum (E2 l1 () l2 ()) where
  toEnum 0 = E21 (label @l1 ())
  toEnum 1 = E22 (label @l2 ())
  toEnum _ = error "E2: toEnum"

  fromEnum (E21 _) = 0
  fromEnum (E22 _) = 1

-- | Map a separate function over every possibility in this enum. The label names stay the same.
--
-- Each function has access to its label, this is intentional so that you have to mention the label once (e.g. by using dot-notation), to prevent confusing the cases.
mapE2 ::
  forall l1 t1 t1' l2 t2 t2'.
  (Label l1 t1 -> t1') ->
  (Label l2 t2 -> t2') ->
  E2 l1 t1 l2 t2 ->
  E2 l1 t1' l2 t2'
mapE2 f1 f2 = \case
  E21 lbl -> lbl & getLabel @l1 & f1 & label @l1 & E21
  E22 lbl -> lbl & getLabel @l2 & f2 & label @l2 & E22

-- | Map a single function over every possiblity in this enum. All fields have to have the same type.
monoMapE2 :: (t -> t') -> E2 l1 t l2 t -> E2 l1 t' l2 t'
monoMapE2 f = \case
  E21 lbl -> lbl & mapLabel f & E21
  E22 lbl -> lbl & mapLabel f & E22

-- | If ever branch of this enum has the same type, fold the enum into its contents.
-- This loses the distinction between cases.
monoFoldE2 :: E2 l1 t l2 t -> t
monoFoldE2 = \case
  E21 (Label t) -> t
  E22 (Label t) -> t

-- | Partition a list of E2 into two lists that each keep their respective label.
-- Like 'partitionEithers', but with labels.
partitionE2 :: forall l1 t1 l2 t2. [E2 l1 t1 l2 t2] -> T2 l1 [t1] l2 [t2]
partitionE2 es =
  es
    <&> ( \case
            E21 (Label t1) -> Left t1
            E22 (Label t2) -> Right t2
        )
    & partitionEithers
    & (\(t1s, t2s) -> T2 (label @l1 t1s) (label @l2 t2s))

-- | Map a monadic (actually just a functor-ic) function over each possibility in this enum. All fields have to have the same type.
monoTraverseE2 :: (Functor f) => (t -> f t') -> E2 l1 t l2 t -> f (E2 l1 t' l2 t')
monoTraverseE2 f = \case
  E21 lbl -> lbl & traverseLabel f <&> E21
  E22 lbl -> lbl & traverseLabel f <&> E22

-- | Check the E21 case. Use TypeApplications to make sure you are checking the right case.
--
-- >>> isE21 @"foo" (E21 (label @"foo" 'c') :: E2 "foo" Char "bar" Int)
-- True
isE21 :: forall l1 t1 l2 t2. E2 l1 t1 l2 t2 -> Bool
isE21 = \case
  E21 _ -> True
  E22 _ -> False

-- | Check the E22 case. Use TypeApplications to make sure you are checking the right case.
--
-- >>> isE22 @"bar" (E21 (label @"foo" 'c') :: E2 "foo" Char "bar" Int)
-- False
isE22 :: forall l2 t2 l1 t1. E2 l1 t1 l2 t2 -> Bool
isE22 = \case
  E21 _ -> False
  E22 _ -> True

getE21 :: forall l1 t1 l2 t2. E2 l1 t1 l2 t2 -> Maybe t1
getE21 = \case
  E21 lbl -> Just $ getField @l1 lbl
  E22 _ -> Nothing

getE22 :: forall l2 t2 l1 t1. E2 l1 t1 l2 t2 -> Maybe t2
getE22 = \case
  E21 _ -> Nothing
  E22 lbl -> Just $ getField @l2 lbl

-- | A named 3-alternative sum (“'Either' with labels”).
data E3 (l1 :: Symbol) t1 (l2 :: Symbol) t2 (l3 :: Symbol) t3
  = E31 (Label l1 t1)
  | E32 (Label l2 t2)
  | E33 (Label l3 t3)
  deriving stock (Eq, Show)

instance (Bounded t1, Bounded t3) => Bounded (E3 l1 t1 l2 t2 l3 t3) where
  minBound = E31 (label @l1 minBound)
  maxBound = E33 (label @l3 maxBound)

-- TODO: instance for arbitrary Enum types?
instance Enum (E3 l1 () l2 () l3 ()) where
  toEnum 0 = E31 (label @l1 ())
  toEnum 1 = E32 (label @l2 ())
  toEnum 2 = E33 (label @l3 ())
  toEnum _ = error "E3: toEnum"

  fromEnum (E31 _) = 0
  fromEnum (E32 _) = 1
  fromEnum (E33 _) = 2

-- | Map a function over every element in this enum. The label names stay the same.
mapE3 ::
  forall l1 t1 t1' l2 t2 t2' l3 t3 t3'.
  (Label l1 t1 -> t1') ->
  (Label l2 t2 -> t2') ->
  (Label l3 t3 -> t3') ->
  E3 l1 t1 l2 t2 l3 t3 ->
  E3 l1 t1' l2 t2' l3 t3'
mapE3 f1 f2 f3 = \case
  E31 lbl -> lbl & getLabel @l1 & f1 & label @l1 & E31
  E32 lbl -> lbl & getLabel @l2 & f2 & label @l2 & E32
  E33 lbl -> lbl & getLabel @l3 & f3 & label @l3 & E33
