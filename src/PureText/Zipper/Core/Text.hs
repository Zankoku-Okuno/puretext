{- |
This is a simple zipper over the 'Text' type.
Since 'Text' has much better performance characteristics than 'String',
    it is perhaps not strictly necessary.
Nevertheless, I have implemented this one for
    the sake of consistency with the other zipper algorithms.

This module contains the core data structures and algorithms for
    making single-point edits within a text buffer.
It is not meant to be imported directly, but instead used through the
    much smaller "PureText.Zipper.Text" interface.
It is provided only in case frontends use this
    as a data structure when there is only a single selection,
    in which case the internal data structure may need to be read to be rendered.
-}
module PureText.Zipper.Core.Text where

import PureText.Zipper.Base
import PureText.Util

import Data.Text (Text)
import qualified Data.Text as T

{- |
A zipper into 'Text', with left (@pre@) and right (@post@) sides stored in-order,
since push\/pop is O(1) on both ends of 'Text'.
-}
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

    pop Forwards TZ{pre, post = c :< post } = Just (c, TZ{pre, post})
    pop Backwards TZ{pre = pre :> c, post } = Just (c, TZ{pre, post})
    pop _ _ = Nothing