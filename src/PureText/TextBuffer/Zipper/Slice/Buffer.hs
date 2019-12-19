{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module PureText.TextBuffer.Zipper.Slice.Buffer where

import PureText.TextBuffer.Zipper.Slice
import PureText.TextBuffer.Lines
import PureText.TextBuffer.Lines.Core

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T


empty :: Monoid a => BufferSlices a
empty = mempty

singleton :: Monoid a => BufferSlice a -> BufferSlices a
singleton = BS . Seq.singleton

doubleton :: Monoid a => BufferSlice a -> BufferSlice a -> BufferSlices a
doubleton x = BS . (x :<|) . Seq.singleton


cons :: Monoid a => BufferSlice a -> BufferSlices a -> BufferSlices a
cons x ys = singleton x <> ys

uncons :: Monoid a => BufferSlices a -> Maybe (BufferSlice a, BufferSlices a)
uncons (BS Empty) = Nothing
uncons (BS (Whole d (x :<<|| xs) :<| ys)) = Just (Whole d (OneLine x), BS ys')
    where ys' = case xs of { Nil -> ys ; _ -> Whole d xs :<| ys }
uncons (BS (x :<| ys)) = Just (x, BS ys)

snoc :: Monoid a => BufferSlices a -> BufferSlice a -> BufferSlices a
snoc xs y = xs <> singleton y

unsnoc :: Monoid a => BufferSlices a -> Maybe (BufferSlices a, BufferSlice a)
unsnoc (BS Empty) = Nothing
unsnoc (BS (xs :|> Whole d (ys :||>> y))) = Just (BS xs', Whole d (OneLine y))
    where xs' = case ys of { Nil -> xs ; _ -> xs :|> Whole d ys }
unsnoc (BS (xs :|> y)) = Just (BS xs, y)


hyperCons :: Monoid a => (BufferSlice a, LineSlices (a, Dirt)) -> BufferSlices a -> BufferSlices a
hyperCons (Central central, fromLineSlices -> Late late) (BS xs) =
    BS $ central :<| late :<| xs
hyperCons _ _ = error "precondition violation: invalid hyperline detected"

hyperUncons :: Monoid a => BufferSlices a -> Maybe (HyperLine a, BufferSlices a)
hyperUncons (BS (Early (Cutup early) :<| Central central :<| Late (Cutup late) :<| rest)) =
    Just ((early, central, late), BS rest)
hyperUncons (BS (Early _ :<| _)) = error "invariant violation: Early without following Central"
hyperUncons (BS (Central _ :<| _)) = error "invariant violation: Central without preceding Early"
hyperUncons (BS (Late _ :<| _)) = error "invariant violation: Late without preceding Central"
hyperUncons _ = Nothing

hyperSnoc :: Monoid a => BufferSlices a -> (LineSlices (a, Dirt), BufferSlice a) -> BufferSlices a
hyperSnoc (BS xs) (fromLineSlices -> Early early, Central central) =
    BS $ xs :|> early :|> central
hyperSnoc _ _ = error "precondition violation: invalid hyperline detected"

hyperUnsnoc :: Monoid a => BufferSlices a -> Maybe (BufferSlices a, HyperLine a)
hyperUnsnoc (BS (rest :|> Early (Cutup early) :|> Central central :|> Late (Cutup late))) =
    Just (BS rest, (early, central, late))
hyperUnsnoc (BS (_ :|> Late _)) = error "invariant violation: Late without preceding Central"
hyperUnsnoc (BS (_ :|> Central _)) = error "invariant violation: Central without following late"
hyperUnsnoc (BS (_ :|> Early _)) = error "invariant violation: Early without following Central"
hyperUnsnoc _ = Nothing


fromLineSlices :: Monoid a => LineSlices (a, Dirt) -> BufferSlice a
fromLineSlices LS{ slices = T t :<| Empty, h2o = H2O (h2o, dirt) lb }
    = Whole dirt (OneLine $ L t (H2O h2o lb))
fromLineSlices slices = Cutup slices

fromLine :: Monoid a => Line (a, Dirt) -> BufferSlices a
fromLine (L t (H2O (h2o, dirt) lb)) = singleton $ Whole dirt $ OneLine (L t (H2O h2o lb))

fromLines :: Monoid a => Lines (a, Dirt) -> BufferSlices a
fromLines Nil = BS Empty
fromLines (l :<<|| ls) = fromLine l <> fromLines ls
