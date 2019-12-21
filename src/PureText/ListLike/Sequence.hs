module PureText.ListLike.Sequence where

import PureText.ListLike.Base
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Function
import Data.Maybe
import Data.Tuple
import Prelude (Num(..))
import Data.Monoid
import Data.Functor


instance ListLike (Seq a) where
    type Elem (Seq a) = a
    nil = Seq.empty
    cons = (Seq.<|)
    uncons Seq.Empty = Nothing
    uncons (x Seq.:<| xs) = Just (x, xs)
    singleton = Seq.singleton
    append = (<>)
    splitAt = Seq.splitAt
    take = Seq.take
    drop = Seq.drop
    -- the type variables apparently don't work out…‽
    -- the output type determines the output element, but it's the output element that drives type inference
    -- i.e. multiple output types could have the same output element type
    -- which means map will experience worse overheads
    -- map = (<$>)
    rigidMap = (<$>)
    null = Seq.null
    length = Seq.length
    filter = Seq.filter
    -- exclude :: (Elem f -> Bool) -> f -> f
    partition = Seq.partition
    span = Seq.spanl
    takeWhile = Seq.takeWhileL
    dropWhile = Seq.dropWhileL
    break = Seq.breakl
    -- stripPrefix :: Eq (Elem f) => f -> f -> Maybe f
    -- isPrefixOf :: Eq (Elem f) => f -> f -> Bool

instance SeqLike (Seq a) where
    snoc = (Seq.|>)
    unsnoc Seq.Empty = Nothing
    unsnoc (xs Seq.:|> x) = Just (xs, x)
    splitAtEnd n xs = Seq.splitAt (length xs - n) xs
    takeEnd n xs = Seq.take (length xs - n) xs
    dropEnd n xs = Seq.drop (length xs - n) xs
    spanEnd p xs = swap $ Seq.spanr p xs
    takeWhileEnd = Seq.takeWhileR
    dropWhileEnd = Seq.dropWhileR
    -- breakEnd :: (Elem f -> Bool) -> f -> (f, f)
    -- stripSuffix :: Eq (Elem f) => f -> f -> Maybe f
    -- isSuffixOf :: Eq (Elem f) => f -> f -> Bool
