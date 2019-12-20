-- {-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{- |
Even though the user might make multiple selections,
    all modifications to a text buffer occur only at one place.
This is because a modification might adjust the boundaries of selections.
Edits on multiple selections will have to be done by
    looping a single-point editing function over each selection.

This module presents an abstract interface to the data structures and algorithms
    of "PureText.Zipper.Core.Edit", which are not meant to be used often.
Most of the algorithms are exposed through the 'Zippy' instance of 'EditZipper'.
-}
module PureText.Zipper.Edit
    ( EditZipper
    , ) where

import PureText.Zipper.Core.Edit
import PureText.Zipper.Base
