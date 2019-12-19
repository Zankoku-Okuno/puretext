module PureText.Zipper.LineSlices
    ( LineSlicesZipper
    , hyperZipper, prepForHyperline
    , splitLineSlicesZipper, splitEarlyZipper, splitLateZipper
    ) where

import PureText.Zipper.Core.LineSlices
import PureText.Zipper.Text
import PureText.TextBuffer.Lines.Core
import PureText.TextBuffer.Zipper.Base
import PureText.TextBuffer.Zipper.Slice

import Data.Sequence (Seq(..))
import PureText.Util


splitLineSlicesZipper :: Monoid a => a -> LineSlicesZipper a -> (LineSlices (a, Dirt), LineSlices (a, Dirt), Maybe SelectionInfo)
splitLineSlicesZipper lineInfo LZ{ before, here = splitTextZipper -> (pre, post), after, h2o, bounds } =
    ( LS{ slices = before :|> leftEnd, h2o = H2O (lineInfo, Dirty) True }
    , LS{ slices = rightStart :<| after, h2o = markDirty h2o}
    , bounds'
    )
    where
    (leftEnd, rightStart, bounds') = case bounds of
        One bounds -> (StartSel pre, EndSel post, Just bounds)
        Neither -> (T pre, T post, Nothing)
        Other _ -> error "precondition violation: splitting a LineSlicesZipper at a hyperline boundary (use splitEarlyZipper or splitLateZipper)"


splitEarlyZipper :: Monoid a => a -> LineSlicesZipper a -> (LineSlicesZipper a, Line (a, Dirt))
splitEarlyZipper lineInfo LZ{ before, here = splitTextZipper -> (pre, post), after, h2o, bounds } =
    case (after, bounds) of
        (Empty, Other Backwards) ->
            ( LZ
                { before
                , here = toZipper Backwards pre
                , after = Empty
                , h2o = H2O (lineInfo, Dirty) True
                , bounds
                }
            , L post (markDirty h2o)
            )
        _ -> error "precondition violation: passed non-earlyZip to splitEarlyZipper"

splitLateZipper :: Monoid a => a -> LineSlicesZipper a -> (Line (a, Dirt), LineSlicesZipper a)
splitLateZipper lineInfo LZ{ before, here = splitTextZipper -> (pre, post), after, h2o, bounds } =
    case (before, bounds) of
        (Empty, Other Forwards) ->
            ( L pre (H2O (lineInfo, Dirty) True)
            , LZ
                { before = Empty
                , here = toZipper Forwards post
                , after
                , h2o = markDirty h2o
                , bounds
                }
            )
        _ -> error "precondition violation: passed non-lateZip to splitLateZipper"
