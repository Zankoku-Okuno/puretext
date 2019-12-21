{-# LANGUAGE UndecidableInstances #-}
module PureText.Select.Buffer where

import PureText.Prelude
import PureText.Select.Base
import PureText.Select.Line

import PureText.Holding


newtype Buffer a = Buf { unBuf :: Seq (BufferCell a) }
    deriving(Functor, Semigroup, Monoid)
    deriving ListLike via (Holding (Buffer a))
    deriving SeqLike via (Holding (Buffer a))

data BufferCell a
    = L (Line a) -- Dirt -- TODO I seem to want to track dirt at this layer
    -- -- the rest of the cases are for optimization only
    -- -- | MlSel (Seq (Lineish a)) Bounds Dirt
    -- -- -- ^ I'm only storing dirt for the entire selection, b/c the entire selection is likely to get dirty at once
    -- -- | Ls (Seq (Lineish a)) Dirt
    -- -- -- ^ Ls is mere optimization, though when searching for selections, it could be handly
    deriving(Functor)


instance Monoid a => Hold (Buffer a) where
    type Hand (Buffer a) = ()
    type Held (Buffer a) = Seq (BufferCell a)
    grip = const Buf
    ungrip = ((),) . unBuf
instance Monoid a => HandsFree (Buffer a) where
    gripZero = Buf

-- instance ListLike (AsCharbreaks (Buffer a)) Charbreak where

-- instance ListLike (AsChars (BufferCell a)) Char where
-- instance ListLike (AsCharish (BufferCell a)) Charish where
