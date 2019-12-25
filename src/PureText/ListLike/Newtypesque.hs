module PureText.ListLike.Newtypesque where

import PureText.ListLike.Base
import Util.DerivingVia.Newtypesque

import Data.Function
import Data.Functor
import Control.Arrow


instance HasElems f => HasElems (Newtypesque f) where
    type Elem (Newtypesque f) = Elem f

instance ListCore f => ListCore (Newtypesque f) where
    nil = Newtype nil
    cons x (Newtype xs) = Newtype (cons x xs)
    singleton x = Newtype (singleton x)
    append (Newtype xs) (Newtype ys) = Newtype (append xs ys)
    uncons = (second Newtype <$>) . uncons . unNewtype
    null = null . unNewtype

instance SeqCore f => SeqCore (Newtypesque f) where
    snoc (Newtype xs) x = Newtype (snoc xs x)
    unsnoc = (first Newtype <$>) . unsnoc . unNewtype

instance Foldable f => Foldable (Newtypesque f) where
    foldr f z = foldr f z . unNewtype
    foldl f z = foldl f z . unNewtype
    -- FIXME also do the rest of the methods, in case there might be performance improvements

instance ListLike f => ListLike (Newtypesque f) where
    take n = Newtype . take n . unNewtype
    drop n = Newtype . drop n . unNewtype
    span p = (first Newtype . second Newtype) . span p . unNewtype
    -- FIXME also do the rest of the methods, in case there might be performance improvements
instance SeqLike f => SeqLike (Newtypesque f) where
    splitAtEnd n = (first Newtype . second Newtype) . splitAtEnd n . unNewtype
    -- FIXME also do the rest of the methods, in case there might be performance improvements