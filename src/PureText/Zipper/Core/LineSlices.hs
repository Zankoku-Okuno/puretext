{- |
Although we might like to use only 'TextZipper' for our zippers into a line of text in a buffer,
    during editing we also need to keep track of user selections.
    (See "PureText.Zipper.Edit" for what editing is as far as this module is concerned.)
This means that each line may be composed of selected and unselected stretches of text,
    which is implemented with 'LineSlices',
    and for which 'LineSlicesZipper' is the corresponding zipper.

This module contains the core data structures and algorithms for
    making edits within a line sliced by selections.
It is not meant to be imported directly, but instead used through the
    much smaller "PureText.Zipper.LineSlices" interface.
It is provided only in case frontends use this
    as a data structure when there is only a single selection,
    in which case the internal data structure may need to be read to be rendered.
-}
module PureText.Zipper.Core.LineSlices where

import PureText.Zipper.Base
import PureText.Slice.Core
import PureText.Zipper.Text
import PureText.Lines
import qualified PureText.Slice.Buffer as B

import Control.Applicative

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import PureText.Util

{-note that it's possible to have only a StartSel/EndSel come in,
    in which case here is empty, and the edit cursor is on the line but outside the selection
    (and therefore adjacent to a newline)
-}
{- |
A zipper over line slices.
Each time its carriage advances, it may advance over a character,
    or over an inline selection boundary.
It will not, however, move across the boundary of a multi-line selection.
That is why @'Blocked' ('LineSlicesZipper' a)@ is @'Neither' 'GoingHyperLine'@:
    when the end of the line is reached normally, then moving the carriage will result in a 'Neither',
    but when it reached the edge of a multi-line selection, if will result in 'Other GoingHyperLine'.
The naming here follows the hyperspace analogy laid out in (FIXME where am I going to describe this analogy?).

If this is being used as a zipper into an edge line of a 'HyperLine',
    then the @bounds@ field will be @'Other' 'Backwards'@ (for the first line)
    or @'Other' 'Forwards'@ (for the last line).

INVARIANT: 'StartSel' and 'EndSel' can only appear at the end\/start of @after@\/@before@, respectively.
    This corresponds to a similar invariant on 'LineSlices'.

INVARIANT: If @bounds@ is @'Other' 'Forwards'@, @before@ is 'Empty'.
    From the other direction, if @bounds@ is @'Other' 'Backwards'@, @after@ is 'Empty'.
-}
data LineSlicesZipper a = LZ
    { before :: Seq LineSlice
    , here :: TextZipper
    , after :: Seq LineSlice
    , h2o :: LineHydration (a, Dirt)
    , bounds :: Neither Direction SelectionInfo -- One is inline, Other is multiline, Neither is no selection
    -- ^ give the selection bounds if appropriate,
    --   or in the 'Other' case, note which side of the center of a multi-line selection this edge line is on
    }


instance Monoid a => Zippy (LineSlicesZipper a) where
    type Base (LineSlicesZipper a) = LineSlices (a, Dirt)
    type Blocked (LineSlicesZipper a) = Neither GoingHyperLine
    type Elem (LineSlicesZipper a) = Char

    -- this implementation avoids entering any multi-line selection
    toZipper dir LS{slices, h2o} = LZ{ before, here, after, h2o, bounds = Neither }
        where
        (before, here, after) = fromSeq dir slices
        -- detect exiting from a multi-line selection
        fromSeq Forwards (sel@(EndSel _) :<| rest) =
            let (before, here, after) =  fromSeq' Forwards rest
            in (sel :<| before, here, after)
        fromSeq Backwards (rest :|> sel@(StartSel _)) =
            let (before, here, after) = fromSeq' Backwards rest
            in (before, here, after :|> sel)
        fromSeq dir xs = fromSeq' dir xs
        -- then deal with opening/simulating T constructors
        fromSeq' Forwards (T here :<| after) = (Empty, toZipper Forwards here, after)
        fromSeq' Forwards _ = (Empty, toZipper dir "", slices)
        fromSeq' Backwards (before :|> T here) = (before, toZipper dir here, Empty)
        fromSeq' Backwards _ = (slices, toZipper dir "", Empty)

    fromZipper LZ{before, here, after, h2o, bounds} = case hereSlice of
        T "" -> LS (before <> after) h2o
        _ -> LS (before <> Seq.singleton hereSlice <> after) h2o
        where
        hereSlice = case bounds of
            One bounds -> InlSel (fromZipper here) bounds
            Other Backwards -> StartSel (fromZipper here)
            Other Forwards -> EndSel (fromZipper here)
            Neither -> T (fromZipper here)

    moveCarriage dir z@LZ{before, here, after, h2o, bounds} = easyMode <|> hardMode
        where
        easyMode = maybeToOne $ (\here -> z{here}) <$> moveCarriage dir here
        hardMode = do
            hereSlice <- case bounds of
                One bounds -> One $ InlSel (fromZipper here) bounds
                Neither -> One $ T (fromZipper here)
                Other _ -> Neither
            let earlier = case (dir, before, hereSlice, after) of
                    (Forwards, before, T "", _) -> before
                    (Forwards, before, last, _) -> before :|> last
                    (Backwards, _, T "", after) -> after
                    (Backwards, _, last, after) -> last :<| after
            (next, further, bounds') <- case (dir, before, after) of
                (Forwards, _, T t :<| further) -> One (t, further, Neither)
                (Forwards, _, InlSel t bounds' :<| further) -> One (t, further, One bounds')
                (Forwards, _, StartSel t :<| Empty) -> Other GoHyper
                (Forwards, _, Empty) -> Neither
                (Backwards, further :|> T t, _) -> One (t, further, Neither)
                (Backwards, further :|> InlSel t bounds', _) -> One (t, further, One bounds')
                (Backwards, Empty :|> EndSel t, _) -> Other GoHyper
                (Backwards, Empty, _) -> Neither
                _ -> error "invariant violation"
            case dir of
                Forwards -> One LZ
                    { before = earlier
                    , here = toZipper dir next
                    , after = further
                    , h2o
                    , bounds = bounds'
                    }
                Backwards -> One LZ
                    { before = further
                    , here = toZipper dir next
                    , after = earlier
                    , h2o
                    , bounds = bounds'
                    }

    push dir c z@LZ{here, h2o} = z{here = push dir c here, h2o = markDirty h2o}

    pop dir z@LZ{before, here, after, h2o} = case pop dir here of
        Just (c, here) -> One (c, z{here, h2o = markDirty h2o})
        Nothing -> case (dir, before, after) of
            (Forwards, _, Empty) -> Neither
            (Backwards, Empty, _) -> Neither
            _ -> Other GoHyper -- WARNING I'm abusing this hyperspace thing here;
                                -- I'm using it here only so say pop has hit a selection boundary


------------ Entering HyperLine Zippers ------------

{-|
Create a zipper for use as an edge of a 'PureText.Zippers.HyperLine.HyperLineZipper'.
That means that the @here@ field will be set to the contents of the 'StartSel' or 'EndSel'
    (with invariants maintained appropriately).

In contrast, using the 'toZipper' method will skip over a 'StartSel' of 'EndSel' at the edge of the line,
    instead beginning from the nearest slice that is not part of a multi-line selection.
-}
hyperZipper :: Direction -> LineSlices (a, Dirt) -> LineSlicesZipper a
hyperZipper Forwards LS{slices = before :|> StartSel here, h2o} = LZ
    { before
    , here = toZipper Forwards here
    , after = Empty
    , bounds = Other Backwards
    , h2o
    }
hyperZipper Backwards LS{slices =  EndSel here :<| after, h2o} = LZ
    { before = Empty
    , here = toZipper Backwards here
    , after
    , bounds = Other Forwards
    , h2o
    }

{- |
Helper that turns a 'LineSlicesZipper' into 'BufferSlices' to ease moving into a 'HyperLine'.
The naming here follows the hyperspace analogy laid out in (FIXME where am I going to describe this analogy?).

PRECONDITION: the input 'LineSlicesZipper' is the edge of a multi-line selection
    (i.e. starts\/ends with 'EndSel' of 'StartSel' resp.).

POSTCONDITION: the Tesulting 'BufferSlices' is a single 'Cutup'
    that ends\/starts with a 'StartSel' or 'EndSel' respectively.
-}
prepForHyperline :: Monoid a => LineSlicesZipper a -> BufferSlices a
prepForHyperline = B.singleton . Cutup . fromZipper
