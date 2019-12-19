module PureText.Zipper.Core.LineSlices where

import PureText.TextBuffer.Zipper.Base
import PureText.TextBuffer.Zipper.Slice
import PureText.Zipper.Text
import PureText.TextBuffer.Lines
import qualified PureText.TextBuffer.Zipper.Slice.Buffer as B

import Control.Applicative

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import PureText.Util


{- INVARIANT: if bounds is Other Forwards, before is Empty; if bounds is Other Backwards, after is Empty -}
data LineSlicesZipper a = LZ
    { before :: Seq LineSlice
    , here :: TextZipper
    , after :: Seq LineSlice
    , h2o :: LineHydration (a, Dirt)
    , bounds :: Neither Direction SelectionInfo -- One is inline, Other is multiline, Neither is no selection
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


------------ Entering HyperLine Zippers ------------

-- this function is for entering a HyperLine
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

prepForHyperline :: Monoid a => LineSlicesZipper a -> BufferSlices a
prepForHyperline = B.singleton . Cutup . fromZipper
