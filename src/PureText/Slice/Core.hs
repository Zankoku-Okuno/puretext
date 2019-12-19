module PureText.Slice.Core where

import PureText.Lines
import PureText.Lines.Core

import Control.Arrow

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T


------------ Buffer Slicing ------------

{- INVARIANTS
    If a BufferSlices has an MlSel, then
        there is a Cutup BufferSlice immediately after which starts with EndSel LineSlice.
    Conversely, if a BufferSlices has a Cutup which starts with an EndSel LineSlice,
        then immediately before there must be a MlSel BufferSlice.

    MlSel has a Nil central for a multi-line selection that spans only two lines.

    A Cutup BufferSlice which ends in a StartSel LineSlice must have
        a MlSel BufferSlice immediately after.
    Conversely, an MlSel BufferSlice must be preceded by
        a CutupSlice which ends in a StartSel LineSlice.
-}
newtype BufferSlices a = BS (Seq (BufferSlice a))

data BufferSlice a
    = Whole Dirt (Lines a)
    | Cutup (LineSlices (a, Dirt))
    | MlSel
        { central :: Lines (a, Dirt)
        , bounds :: SelectionInfo
        }

instance Semigroup a => Semigroup (BufferSlices a) where
    (BS (xs :|> Whole dirt x)) <> (BS (Whole dirt' y :<| ys))
        | dirt == dirt' = BS $ xs <> Seq.singleton (Whole dirt (x <> y)) <> ys
    (BS xs) <> (BS ys) = BS $ xs <> ys
instance Monoid a => Monoid (BufferSlices a) where
    mempty = BS $ mempty

{-
rehydrate :: Seq LineSlice -> (LineHydration, Dirt) -> BufferSlice
rehydrate (It t :<| Empty) (h2o, dirt) = Whole dirt (OneLine $ L t h2o)
rehydrate ls (h2o, dirt) = Cutup ls h2o dirt
-}

data Dirt = Clean | Dirty
    deriving(Eq, Read, Show)
instance Semigroup Dirt where x <> y = if x == Dirty || y == Dirty then Dirty else Clean
instance Monoid Dirt where mempty = Clean

markDirty :: LineHydration (a, Dirt) -> LineHydration (a, Dirt)
markDirty h2o = second (const Dirty) <$> h2o


------------ Line Slices ------------

{- INVARIANT: Two T LineSlices are never adjacent. -}
data LineSlices a = LS
    { slices :: Seq LineSlice
    , h2o :: LineHydration a
    }
    deriving(Functor)

{- INVARIANT: T LineSlices are never empty. -}
data LineSlice
    = T Text
    | InlSel Text SelectionInfo
    | StartSel Text
    | EndSel Text


sliceLine :: Line a -> LineSlices a
sliceLine (L t h2o) = LS{ slices = Seq.singleton $ T t, h2o }


instance Semigroup a => Semigroup (LineSlices a) where
    LS{slices = xs :|> T x, h2o} <> LS{slices = T y :<| ys, h2o = h2o'}
        = LS{slices = xs <> (Seq.singleton . T) (x <> y) <> ys, h2o = h2o <> h2o'}
    LS{slices = xs, h2o} <> LS{slices = ys, h2o = h2o'}
        = LS{slices = xs <> ys, h2o = h2o <> h2o'}
instance Monoid a => Monoid (LineSlices a) where
    mempty = LS{slices = mempty, h2o = mempty}

------------ Hyper Lines ------------

type HyperLine a = (LineSlices (a, Dirt), BufferSlice a, LineSlices (a, Dirt))

pattern Early :: BufferSlice a -> BufferSlice a
pattern Early x <- x@(Cutup LS{slices = _ :|> StartSel _})

pattern Central :: BufferSlice a -> BufferSlice a
pattern Central x <- x@(MlSel{})

pattern Late :: BufferSlice a -> BufferSlice a
pattern Late x <- x@(Cutup LS{slices = EndSel _ :<| _})


------------ Selection Markings ------------


type SelectionInfo = (SelectionSide, SelectionSide) -- FIXME one has to be Anchor, the other Cursor


data SelectionSide = Anchor | Cursor ColIx


-- FIXME move this elsewhere
data ColIx
    = DynCur
    | FixCur Int
    | End
