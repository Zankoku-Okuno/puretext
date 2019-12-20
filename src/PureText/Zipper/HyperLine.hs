{- |
When a selection crosses multiple lines,
    it is useful to have fingers to the start\/end of the line
    as well as to the start\/end of the selection.
See "PureText.Zipper.Edit" for why we need a zipper inside a 'HyperLine'.

This module presents an abstract interface to the data structures and algorithms
    of "PureText.Zipper.Core.HyperLine", which are not meant to be used often.
Most of the algorithms are exposed through the 'Zippy' instance of 'HyperLineZipper'.
-}
module PureText.Zipper.HyperLine
    ( HyperLineZipper
    , splitInlSel
    ) where

import PureText.Zipper.Core.HyperLine
import PureText.Zipper.Base
-- TODO eliminate as many imports of Core modules as I reasonably can
import PureText.Slice.Core
import PureText.Lines

{- | When a linebreak is added into an 'InlSel', that selection must become a 'HyperLine'.
This function adapts the two sides of the originating 'InlSel' to a 'HyperLineZipper'.

The direction convention here is the same as that for 'push'ing elements:
    treat the 'SelectionInfo' as if it was the newline being 'push'ed in the 'Direction'.
-}
splitInlSel :: Monoid a => Direction -> SelectionInfo
                        -> (LineSlices (a, Dirt), LineSlices (a, Dirt)) -> HyperLineZipper a
splitInlSel Forwards bounds (pre, post) = EarlyMlZ
    { earlyZip = toZipper Backwards pre
    , central = Nil
    , late = post
    , bounds
    }
splitInlSel Backwards bounds (pre, post) = LateMlZ
    { early = pre
    , central = Nil
    , lateZip = toZipper Forwards post
    , bounds
    }
