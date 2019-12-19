module PureText.TextBuffer.Zipper.Text where
    -- ( TextZipper -- FIXME
    -- ) where

import PureText.TextBuffer.Zipper.Base
import PureText.Util

import Data.Text (Text)
import qualified Data.Text as T


data TextZipper = TZ{pre :: Text, post :: Text}


instance Zippy TextZipper where
    type Base TextZipper = Text
    type Blocked TextZipper = Maybe
    type Elem TextZipper = Char

    toZipper Forwards t = TZ "" t
    toZipper Backwards t = TZ t ""

    fromZipper TZ{pre, post} = pre <> post

    moveCarriage Forwards (TZ{pre, post = c :< post}) = Just TZ
        { pre = pre :> c
        , post
        }
    moveCarriage Backwards (TZ{pre = pre :> c, post}) = Just TZ
        { pre
        , post = c :< post
        }
    moveCarriage Forwards (TZ{post = ""}) = Nothing
    moveCarriage Backwards (TZ{pre = ""}) = Nothing

    push Forwards c TZ{pre, post} = TZ{pre, post = c :< post}
    push Backwards c TZ{pre, post} = TZ{pre = pre :> c, post}
