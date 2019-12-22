{-# LANGUAGE UndecidableInstances #-}
module PureText.Select.Buffer where

import PureText.Prelude
import PureText.Select.Base
import PureText.Select.Line
import PureText.Select.LineCell
import PureText.Select.Text

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


fromText :: Monoid a => Text -> Text -> Buffer a
fromText lbseq inp = case AsLines $ go Nil inp of
    x :< xs -> unLines $ mkCur x :< xs
    Nil -> unLines $ singleton $ mkCur Nil
    where
    go !acc inp = case stripInfix lbseq inp of
        Just (pre, post) -> go (acc :> mk pre) post
        Nothing -> acc :> mk inp
    mkCur :: Monoid a => Line a -> Line a
    mkCur line = z :< line
    z = Inl "" Cursor
    mk :: Monoid a => Text -> BufferCell a
    mk = L . singleton . Un . T


instance Monoid a => ListLike (AsLines (Buffer a)) where
    type Elem (AsLines (Buffer a)) = Line a
    nil = AsLines nil
    cons x (AsLines xs) = AsLines $ L x `cons` xs
    uncons (AsLines Nil) = Nothing
    uncons (AsLines (L x :< xs)) = Just (x, AsLines xs)
