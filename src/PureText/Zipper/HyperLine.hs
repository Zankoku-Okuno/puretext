module PureText.Zipper.HyperLine
    ( HyperLineZipper
    , splitInlSel
    ) where

import PureText.Zipper.Core.HyperLine
import PureText.Zipper.Base
-- TODO eliminate as many imports of Core modules as I reasonably can
import PureText.Slice.Core
import PureText.Lines

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
