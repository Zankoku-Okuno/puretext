module PureText.Zipper.HyperLine
    ( HyperLineZipper
    , splitInlSel
    ) where

import PureText.Zipper.Core.HyperLine
import PureText.TextBuffer.Zipper.Base
import PureText.TextBuffer.Zipper.Slice
import PureText.TextBuffer.Lines

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
