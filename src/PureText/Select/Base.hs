module PureText.Select.Base where

import Prelude (error)
import PureText.Prelude
import qualified Data.Map as Map


-- TODO use later
-- data Dirt = Clean | Dirty



data Bounds
    = Cursor
    | Select (Either ColIx ColIx)
    deriving (Eq)
data ColIx
    = FloatCol
    | FixedCol Int
    | FloatEnd
    deriving (Eq)


newtype Marks = Marks { unMarks :: Map (MarkName, MarkIndex) MarkData }
type MarkName = Either Word Char
type MarkIndex = Word -- keep track of which marks match up with which others
type MarkData = (MarkSide, Bounds)
data MarkSide = OpenMark | CloseMark | PointMark
instance Semigroup Marks where
    (Marks a) <> (Marks b) = Marks $ Map.unionWith f a b
        where
        f (_, b1) (_, b2) | b1 /= b2 = error "invariant violation: mark bounds do not match"
        f (OpenMark, bounds) (CloseMark, _) = (PointMark, bounds)
        f (CloseMark, bounds) (OpenMark, _) = (PointMark, bounds) -- FIXME: really?
        f _ _ = error "invariant violation: marks were not conserved"
instance Monoid Marks where
    mempty = Marks $ Map.empty

type MarksOr a = Either Marks a




data Charbreak = C Char | Lb


newtype AsChars t = AsChars { unChars :: t }
    deriving(Semigroup, Monoid)
instance HasElems (AsChars t) where
    type Elem (AsChars t) = Char

newtype AsCharish t = AsCharish { unCharish :: t}
    deriving(Semigroup, Monoid)

newtype AsCharbreaks t = AsCharbreaks { unCharbreaks :: t }
    deriving(Semigroup, Monoid)

newtype AsLines t = AsLines { unLines :: t }
    deriving(Semigroup, Monoid)


{-
TODO

class (ListLike f) => MarkedList f where
    dropAll :: f -> f


newtype Marked f = Marked f
instance HasElem f => HasElem (Marked f) where
    type Elem (Marked f) = Elem f
instance ListCore f => ListCore (Marked f) where
…
…
…
instance ListLike f => MarkedList (Marked f) where

-}
