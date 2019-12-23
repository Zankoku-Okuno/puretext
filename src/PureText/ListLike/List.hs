module PureText.ListLike.List where

import PureText.ListLike.Base
import Prelude (fromIntegral)
import qualified Data.List as List
import qualified Data.Foldable as Fold

import Data.Function
import Data.Maybe
import Data.Monoid

instance HasElems [a] where
    type Elem [a] = a

instance ListCore [a] where
    nil = []
    cons = (:)
    append = (<>)
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
    null = List.null

instance Foldable [a] where
    foldMap = Fold.foldMap
    fold = Fold.fold
    foldr = Fold.foldr
    foldl = Fold.foldl
    foldr' = Fold.foldr'
    foldl' = Fold.foldl'
    foldr1 = Fold.foldr1
    foldl1 = Fold.foldl1
    -- map = List.map -- FIXME why does this not work?
    filter = List.filter
    partition = List.partition
    toList = id
    fromList = id

instance ListLike [a] where
    length = fromIntegral . List.length
    splitAt n = List.splitAt (fromIntegral n)
    take n = List.take (fromIntegral n)
    drop n = List.drop (fromIntegral n)
    span = List.span
    takeWhile = List.takeWhile
    dropWhile = List.dropWhile
    break = List.break
    stripPrefix = List.stripPrefix
    isPrefixOf = List.isPrefixOf
    tails = toList . List.tails
