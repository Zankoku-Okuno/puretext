{-# LANGUAGE UndecidableInstances #-}
module PureText.Select.Line where
{-
    ( Line
    ) where

import PureText.Prelude

import PureText.Select.Base
import PureText.Select.LineCell

import PureText.Holding


data Line a = L
    { cells :: LineCells
    , h2o :: a
    -- , dirt :: Dirt -- TODO? or have this at the buffer level?
    -- , charCount :: Int -- TODO
    }
    deriving(Functor)
    deriving ListLike via (Holding (Line a))
    deriving SeqLike via (Holding (Line a))


instance Monoid a => Hold (Line a) where
    type Held (Line a) = LineCells
    type Hand (Line a) = a
    grip h2o cells = L{cells, h2o}
    ungrip L{cells, h2o} = (h2o, cells)
instance Monoid a => HandsFree (Line a) where
    gripZero cells = L{cells, h2o = mempty}

instance Monoid a => Semigroup (Line a) where
    (L xs h2o) <> (L ys h2o') = L{ cells = xs <> ys, h2o = h2o <> h2o' }
instance Monoid a => Monoid (Line a) where
    mempty = L{ cells = mempty, h2o = mempty }




-- instance ListLike (AsChars (Line a)) Char where
-- instance ListLike (AsCharish (Line a)) Charish where

-}