{- |
Although we might like to use only 'TextZipper' for our zippers into a line of text in a buffer,
    during editing we also need to keep track of user selections.
    (See "PureText.Zipper.Edit" for what editing is as far as this module is concerned.)
This means that each line may be composed of selected and unselected stretches of text,
    which is implemented with 'LineSlices',
    and for which 'LineSlicesZipper' is the corresponding zipper.

This module presents an abstract interface to the data structures and algorithms
    of "PureText.Zipper.Core.LineSlices", which are not meant to be used often.
Most of the algorithms are exposed through the 'Zippy' instance of 'LineSlicesZipper'.
-}
module PureText.Zipper.LineSlices
    ( LineSlicesZipper
    , hyperZipper, prepForHyperline
    , splitLineSlicesZipper, splitEarlyZipper, splitLateZipper
    ) where

import PureText.Zipper.Core.LineSlices
import PureText.Zipper.Text
import PureText.Lines.Core
import PureText.Zipper.Base
import PureText.Slice.Core

import Data.Sequence (Seq(..))
import PureText.Util


{- | When a linebreak is added into a single-line zipper, we must re-arrange the peices.
This function splits a 'LineSlicesZipper' into its two sides and its selection (if it has one).
These can then be re-combined,
    either moving one half into a new line,
    or creating a 'HyperLine'.

PRECONDITION: the input 'LineSlicesZipper' must not already be part of a 'PureText.Zipper.HyperLine.HyperLineZipper'
    (i.e. its @bounds@ should not be 'Other').
    For that, see 'splitEarlyZipper' and 'splitLateZipper'.
-}
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

{- | When a linebreak is added into the first line of a 'PureText.Zipper.HyperLine.HyperLineZipper',
    we have to push part of this zipper into the center of the 'HyperLine'.
This function splits a 'LineSlicesZipper' into its two sides,
    conveniently packaging the parts for reconstruction.

PRECONDITION: the input 'LineSlicesZipper' must be used as the first part of a 'PureText.Zipper.HyperLine.HyperLineZipper'
    (i.e. its @bounds@ should be 'Other Backwards').
    For other cases, see 'splitLineSlicesZipper' and 'splitLateZipper'.
-}
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

{- | When a linebreak is added into the last line of a 'PureText.Zipper.HyperLine.HyperLineZipper',
    we have to push part of this zipper into the center of the 'HyperLine'.
This function splits a 'LineSlicesZipper' into its two sides,
    conveniently packaging the parts for reconstruction.

PRECONDITION: the input 'LineSlicesZipper' must be used as the last part of a 'PureText.Zipper.HyperLine.HyperLineZipper'
    (i.e. its @bounds@ should be 'Other Forwards').
    For other cases, see 'splitLineSlicesZipper' and 'splitEarlyZipper'.
-}
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
