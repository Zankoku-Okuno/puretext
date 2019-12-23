{-# LANGUAGE UndecidableInstances #-}
module PureText.Select.Text
    ( Textish(..)
    ) where

import PureText.Prelude
import Util.DerivingVia.Newtypesque

import PureText.Select.Base


-- newtype Textish = T { unT :: Seq (Marked Text) } -- TODO
newtype Textish = T { unT :: Text }
    deriving
        ( Eq, Ord
        , Semigroup, Monoid
        , HasElems, ListCore, SeqCore, Foldable, ListLike, SeqLike
        )
    deriving Show via (Newtypesque Textish)
    -- deriving Read via (Holding Textish)
    deriving IsString via (Newtypesque Textish)
instance IsNewtypesque Textish Text where
    injNewtype = T
    prjNewtype = unT




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
