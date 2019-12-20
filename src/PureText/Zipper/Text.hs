{- |
This is a simple zipper over the 'Text' type.
Since 'Text' has much better performance characteristics than 'String',
    it is perhaps not strictly necessary.
Nevertheless, I have implemented this one for
    the sake of consistency with the other zipper algorithms.

This module presents an abstract interface to the data structures and algorithms
    of "PureText.Zipper.Core.Text", which are not meant to be used often.
Most of the algorithms are exposed through the 'Zippy' instance of 'TextZipper'.
-}
module PureText.Zipper.Text
    ( TextZipper
    , splitTextZipper
    ) where

import PureText.Zipper.Core.Text
import PureText.Zipper.Base
import Data.Text (Text)


{- |
"PureText" stores its linebreaks in an abstract format, independent of the in-line text,
so that linebreaks may be treated platform-agnostic.
This means that adding a linebreak into a 'TextZipper' will require
splitting the text and re-assembling the lines.

This function gives the left and right halves of the 'TextZipper' (in that order).
-}
splitTextZipper :: TextZipper -> (Text, Text)
splitTextZipper TZ{pre, post} = (pre, post)
