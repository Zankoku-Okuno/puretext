{-# LANGUAGE UndecidableInstances #-}
module PureText.ListLike.Holding where

import PureText.ListLike.Base
import PureText.Holding

import Data.Function
import Data.Tuple
import Data.Maybe
import Data.Functor
import Control.Arrow

{- These instances require undecidable instances.
Realistically, Elem a should be smaller than a, for any a.
If not, it sounds like you're… using ListLike to model set theory without the axiom of foundation‽
Unfortunately, even users need to enable undecidable isntances.
If only GHC had a way for users to tell it that an associated type family must be strictly decreasing.
-}

instance (ListLike (Held f), HandsFree f) => ListLike (Holding f) where
    type Elem (Holding f) = Elem (Held f)
    nil = Hold $ gripZero nil
    cons x xs = holdMap (cons x) xs
    uncons (Hold it) = second (holdWith it) <$> uncons (theHeld it)

    singleton = Hold . gripZero . singleton
    -- append prefers the hand of the second arg, since that's what the default implementation would do
    append (Hold xs) (Hold ys) = holdWith ys $ append (theHeld xs) (theHeld ys)
    splitAt n (Hold it) =
        let (xs, ys) = splitAt n (theHeld it)
        in (holdWith it xs, holdWith it ys)
    take n = holdMap (take n)
    drop n = holdMap (drop n)
    map f = map f . theHeld . unHold
    rigidMap f = holdMap (rigidMap f)
    null = null . theHeld . unHold
    length = length . theHeld . unHold
    filter p = holdMap (filter p)
    exclude p = holdMap (exclude p)
    partition p (Hold it) =
        let (y, n) = partition p (theHeld it)
        in (holdWith it y, holdWith it n)
    span p (Hold it) =
        let (xs, ys) = span p (theHeld it)
        in (holdWith it xs, holdWith it ys)
    takeWhile p = holdMap (takeWhile p)
    dropWhile p = holdMap (dropWhile p)
    break p (Hold it) =
        let (xs, ys) = break p (theHeld it)
        in (holdWith it xs, holdWith it ys)
    stripPrefix (Hold needle) (Hold haystack) =
        holdWith haystack <$> stripPrefix (theHeld needle) (theHeld haystack)
    isPrefixOf (Hold needle) = isPrefixOf (theHeld needle) . theHeld . unHold
    stripInfix (Hold needle) (Hold haystack) =
        case stripInfix (theHeld needle) (theHeld haystack) of
            Just (xs, ys) -> Just (holdWith haystack xs, holdWith haystack ys)
            Nothing -> Nothing
    isInfixOf (Hold needle) = isPrefixOf (theHeld needle) . theHeld . unHold

instance (SeqLike (Held f), HandsFree f) => SeqLike (Holding f) where
    snoc xs x = holdMap (`snoc` x) xs
    unsnoc (Hold it) = first (holdWith it) <$> unsnoc (theHeld it)
    -- TODO implement the rest, to squeeze any possible performance out
    splitAtEnd n (Hold it) =
        let (xs, ys) = splitAtEnd n (theHeld it)
        in (holdWith it xs, holdWith it ys)
    takeEnd n = holdMap (takeEnd n)
    dropEnd n = holdMap (dropEnd n)
    spanEnd p (Hold it) =
        let (xs, ys) = spanEnd p (theHeld it)
        in (holdWith it xs, holdWith it ys)
    takeWhileEnd p = holdMap (takeWhileEnd p)
    dropWhileEnd p = holdMap (dropWhileEnd p)
    breakEnd p (Hold it) =
        let (xs, ys) = breakEnd p (theHeld it)
        in (holdWith it xs, holdWith it ys)
    stripSuffix (Hold needle) (Hold haystack) =
        holdWith haystack <$> stripSuffix (theHeld needle) (theHeld haystack)
    isSuffixOf (Hold needle) = isSuffixOf (theHeld needle) . theHeld . unHold
