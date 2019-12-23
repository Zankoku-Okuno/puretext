module PureText.ListLike.Sequence where

import PureText.ListLike.Base
import Prelude (fromIntegral)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

import Data.Function
import Data.Bool
import Data.Eq
import Data.Maybe
import Data.Tuple
import Prelude (Num(..))
import Data.Monoid
import Data.Functor

instance HasElems (Seq a) where
    type Elem (Seq a) = a

instance ListCore (Seq a) where
    nil = Seq.Empty
    cons = (Seq.<|)
    singleton = Seq.singleton
    append = (<>)
    uncons Seq.Empty = Nothing
    uncons (x Seq.:<| xs) = Just (x, xs)
    null = Seq.null
instance SeqCore (Seq a) where
    snoc = (Seq.|>)
    unsnoc Seq.Empty = Nothing
    unsnoc (xs Seq.:|> x) = Just (xs, x)

instance Foldable (Seq a) where
    foldMap = Fold.foldMap
    fold = Fold.fold
    foldr = Fold.foldr
    foldl = Fold.foldl
    foldr' = Fold.foldr'
    foldl' = Fold.foldl'
    foldr1 = Fold.foldr1
    foldl1 = Fold.foldl1
    -- map = fmap -- FIXME why does this not work?
    rigidMap = fmap
    filter = Seq.filter
    partition = Seq.partition
    toList = Fold.toList
    fromList = Seq.fromList

instance ListLike (Seq a) where
    length = fromIntegral . Seq.length
    splitAt n = Seq.splitAt (fromIntegral n)
    take n = Seq.take (fromIntegral n)
    drop n = Seq.drop (fromIntegral n)
    span = Seq.spanl
    takeWhile = Seq.takeWhileL
    dropWhile = Seq.dropWhileL
    break = Seq.breakl
    stripPrefix needle haystack
        | needle == prefix = Just suffix
        | otherwise = Nothing
        where (prefix, suffix) = splitAt (length needle) haystack
    tails = toList . Seq.tails
instance SeqLike (Seq a) where
    splitAtEnd n xs = Seq.splitAt (Seq.length xs - n) xs
    spanEnd p = swap . Seq.spanr p
    takeWhileEnd p = Seq.takeWhileR p
    dropWhileEnd p = Seq.dropWhileR p
    breakEnd p = swap . Seq.breakr p
    stripSuffix needle haystack
        | needle == prefix = Just suffix
        | otherwise = Nothing
        where (prefix, suffix) = splitAt (length needle) haystack
    inits = toList . Seq.inits
    reverse = Seq.reverse
