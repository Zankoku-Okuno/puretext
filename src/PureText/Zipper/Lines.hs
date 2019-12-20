{- |
This is a zipper over 'PureText.Lines.Lines', centering on a 'PureText.Zipper.Text.TextZipper'.
It is possibly the most reusable of the zippers in "PureText.Zipper",
    since it does not impose any notion of slicing lines or making selections,
    instead opting to be a simple two-dimensional zipper over lines of text.
Nevertheless, it is only currently used by "PureText.Zipper.HyperLine" at the moment.

This module presents an abstract interface to the data structures and algorithms
    of "PureText.Zipper.Lines", which are not meant to be used often.
Most of the algorithms are exposed through the 'Zippy' instance of 'LinesZipper'.
-}
module PureText.Zipper.Lines
    ( LinesZipper
    ) where

import PureText.Zipper.Core.Lines
import PureText.Zipper.Base
