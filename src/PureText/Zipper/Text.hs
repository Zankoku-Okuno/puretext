module PureText.Zipper.Text
    ( TextZipper
    , splitTextZipper
    ) where

import PureText.Zipper.Core.Text

import Data.Text (Text)


splitTextZipper :: TextZipper -> (Text, Text)
splitTextZipper TZ{pre, post} = (pre, post)
