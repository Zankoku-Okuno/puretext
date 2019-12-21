{-# LANGUAGE UndecidableInstances #-}
module PureText.ListLike.AppendBased
    ( AsAppendBasedList(..)
    , AppendBasedList(..), AppendBasedSeq(..)
    , HoldingAppendBasedList(..)
    ) where

import PureText.ListLike.Base
import PureText.Holding
import PureText.ListLike.Holding

import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Functor
import Control.Arrow


newtype AsAppendBasedList a = AppL { unAppL :: a }


class (Monoid f) => AppendBasedList f where
    data AppendBasedElem f
    appendBasedSingleton :: AppendBasedElem f -> f
    appendBasedCons :: AppendBasedElem f -> f -> f
    appendBasedUncons :: f -> Maybe (AppendBasedElem f, f)

class (AppendBasedList f) => AppendBasedSeq f where
    appendBasedSnoc :: f -> AppendBasedElem f -> f
    appendBasedUnsnoc :: f -> Maybe (f, AppendBasedElem f)


instance (AppendBasedList f) => ListLike (AsAppendBasedList f) where
    type (Elem (AsAppendBasedList f)) = AppendBasedElem f
    nil = AppL mempty
    cons x = AppL . appendBasedCons x . unAppL
    uncons = (second AppL <$>) . appendBasedUncons . unAppL
    singleton = AppL . appendBasedSingleton
    append (AppL xs) (AppL ys) = AppL $ xs <> ys

instance (AppendBasedSeq f) => SeqLike (AsAppendBasedList f) where
    snoc (AppL xs) x = AppL $ appendBasedSnoc xs x
    unsnoc = (first AppL <$>) . appendBasedUnsnoc . unAppL



newtype HoldingAppendBasedList a = HAppL { unHAppL :: a }

instance (ListLike (Held f), HandsFree f, AppendBasedList f) => ListLike (HoldingAppendBasedList f) where
    type (Elem (HoldingAppendBasedList f)) = AppendBasedElem f
    nil = HAppL mempty
    cons x = HAppL . appendBasedCons x . unHAppL
    uncons = (second HAppL <$>) . appendBasedUncons . unHAppL
    singleton = HAppL . appendBasedSingleton
    append (HAppL xs) (HAppL ys) = HAppL $ xs <> ys

    splitAt n it =
        let (xs, ys) = splitAt n (ha it)
        in (ah xs, ah ys)
    take n = holdApMap (take n)
    drop n = holdApMap (drop n)
    null = null . ha
    length = length . ha
    -- these all have the same problem as defining cons in terms of singleton: AppendBasedElem /= Elem
    -- map f = map f . ha -- this has the same problem as 
    -- rigidMap f = holdApMap (rigidMap f) -- this has the same problem as defining cons in terms of singleton
    -- filter p = holdApMap (filter p)
    -- exclude :: (Elem f -> Bool) -> f -> f
    -- partition :: (Elem f -> Bool) -> f -> (f, f)
    -- span :: (Elem f -> Bool) -> f -> (f, f)
    -- takeWhile :: (Elem f -> Bool) -> f -> f
    -- dropWhile :: (Elem f -> Bool) -> f -> f
    -- break :: (Elem f -> Bool) -> f -> (f, f)
    -- stripPrefix :: Eq (Elem f) => f -> f -> Maybe f
    -- isPrefixOf :: Eq (Elem f) => f -> f -> Bool

ah :: Holding a -> HoldingAppendBasedList a
ah = HAppL . unHold
ha :: HoldingAppendBasedList a -> Holding a
ha = Hold . unHAppL
ahha :: (Holding a -> Holding a) -> (HoldingAppendBasedList a -> HoldingAppendBasedList a)
ahha f = ah . f . ha

holdApMap :: Hold a => (Held a -> Held a) -> HoldingAppendBasedList a -> HoldingAppendBasedList a
holdApMap f = ahha (holdMap f)
