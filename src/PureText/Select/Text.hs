{-# LANGUAGE UndecidableInstances #-}
module PureText.Select.Text
    ( Textish(..)
    ) where

import PureText.Prelude
import Util.DerivingVia.Newtypesque

import PureText.Select.Base


-- newtype Textish = T { unT :: Seq (MarkOr Text) } -- TODO
newtype Textish = T { unT :: Text }
    deriving
        ( Eq, Ord
        , Semigroup, Monoid
        , HasElems, ListCore, SeqCore, Foldable, ListLike, SeqLike
        )
    deriving Show via (Newtypesque Textish)
    -- deriving Read via (Holding Textish)
    deriving IsString via (Newtypesque Textish)








-- TODO these need to be altered once Textish is a Marked Text
{- The nice thing is, the compiler will tell me about it -}
instance IsNewtypesque Textish Text where
    injNewtype = T
    prjNewtype = unT
deriving via (Newtypesque Textish) instance ListCore (AsChars Textish)
deriving via (Newtypesque Textish) instance SeqCore (AsChars Textish)
deriving via (Newtypesque Textish) instance Foldable (AsChars Textish)
deriving via (Newtypesque Textish) instance ListLike (AsChars Textish)
deriving via (Newtypesque Textish) instance SeqLike (AsChars Textish)


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
