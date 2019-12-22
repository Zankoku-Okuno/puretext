{-# LANGUAGE UndecidableInstances #-}
module PureText.Select.Text
    ( Textish(..)
    ) where

import PureText.Prelude
import PureText.Holding

import PureText.Select.Base


-- newtype Textish = T { unT :: Seq (Marked Text) } -- TODO
newtype Textish = T { unT :: Text }
    deriving(Eq, Ord, Semigroup, Monoid)
    deriving Show via (Holding Textish)
    -- deriving Read via (Holding Textish)
    deriving IsString via (Holding Textish)
    deriving ListLike via (Holding Textish)
    deriving SeqLike via (Holding Textish)


instance Hold Textish where
    type Held Textish = Text
    type Hand Textish = ()
    grip _ = T
    ungrip = ((),) . unT
instance HandsFree Textish where
    gripZero = T



-- instance IsString Textish where fromString = T . fromString


-- instance ListLike (AsChars Textish) where
--     type Elem (AsChars Textish) = Char
--     singleton c = AsChars . T $ singleton c
--     uncons = (second (AsChars . T) <$>) . uncons . unT . unChars
--     null = null . unT . unChars
--     -- TODO implement the other operations for better performance


-- TODO later
-- type Charish = Marked Char


-- TODO instance ListLike Textish (Either Mark Text) where
-- TODO instance ListLike (AsChars Textish) Char where
-- TODO instance ListLike (AsCharish Textish) Charish where
