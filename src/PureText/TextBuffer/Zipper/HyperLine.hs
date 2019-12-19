module PureText.TextBuffer.Zipper.HyperLine where
    -- ( HyperLineZipper
    -- , HyperLine
    -- ) where

import PureText.TextBuffer.Zipper.Base
import PureText.TextBuffer.Zipper.Slice
import qualified PureText.TextBuffer.Zipper.Slice.Buffer as BS
import PureText.TextBuffer.Zipper.Text
import PureText.TextBuffer.Zipper.LineSlices (LineSlicesZipper, hyperZipper, prepForHyperline)
import qualified PureText.TextBuffer.Zipper.LineSlices as LZ
import PureText.TextBuffer.Zipper.Lines
import PureText.TextBuffer.Lines
import PureText.TextBuffer.Lines.Core
import PureText.Util

import Control.Applicative

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq


data HyperLineZipper a
    = EarlyMlZ
        { earlyZip :: LineSlicesZipper a
        , central :: Lines (a, Dirt)
        , late :: LineSlices (a, Dirt)
        , bounds :: SelectionInfo
        }
    | CentralMlZ
        { early :: LineSlices (a, Dirt)
        , centralZip :: LinesZipper a
        , late :: LineSlices (a, Dirt)
        , bounds :: SelectionInfo
        }
    | LateMlZ
        { early :: LineSlices (a, Dirt)
        , central :: Lines (a, Dirt)
        , lateZip :: LineSlicesZipper a
        , bounds :: SelectionInfo
        }


instance (Monoid a) => Zippy (HyperLineZipper a) where
    type Base (HyperLineZipper a) = HyperLine a
    type Blocked (HyperLineZipper a) = Neither GoingHyperLine
    type Elem (HyperLineZipper a) = Charbreak a

    toZipper Forwards (early, MlSel{central, bounds}, late) = EarlyMlZ
        { earlyZip = hyperZipper Forwards early
        , central
        , bounds
        , late
        }
    toZipper Backwards (early, MlSel{central, bounds}, late) = LateMlZ
        { early
        , central
        , bounds
        , lateZip = hyperZipper Backwards late
        }

    fromZipper EarlyMlZ{earlyZip, central, late, bounds} =
        ( fromZipper earlyZip
        , MlSel{central, bounds}
        , late
        )
    fromZipper CentralMlZ{early, centralZip, late, bounds} =
        ( early
        , MlSel{central = fromZipper centralZip, bounds}
        , late
        )
    fromZipper LateMlZ{early, central, lateZip, bounds} =
        ( early
        , MlSel{central, bounds}
        , fromZipper lateZip
        )

    moveCarriage dir z@EarlyMlZ{earlyZip, central, late, bounds} = easyMode <|> hardMode
        where
        easyMode = (\earlyZip -> z{earlyZip}) <$> moveCarriage dir earlyZip
        hardMode = case (dir, central) of
            (Forwards, _ :<<|| _) -> One $ CentralMlZ
                { early = fromZipper earlyZip
                , centralZip = toZipper dir central
                , late
                , bounds
                }
            (Forwards, Nil) -> One $ LateMlZ
                { early = fromZipper earlyZip
                , central
                , lateZip = hyperZipper Backwards late
                , bounds
                }
            (Backwards, _) -> Other GoHyper
    moveCarriage dir z@CentralMlZ{early, centralZip, late, bounds} = easyMode <|> hardMode
        where
        easyMode = (\centralZip -> z{centralZip}) <$> maybeToOne (moveCarriage dir centralZip)
        hardMode = case dir of
            Forwards -> One $ LateMlZ
                { early
                , central = fromZipper centralZip
                , lateZip = hyperZipper Forwards late
                , bounds
                }
            Backwards -> One $ EarlyMlZ
                { earlyZip = hyperZipper Forwards early
                , central = fromZipper centralZip
                , late
                , bounds
                }
    moveCarriage dir z@LateMlZ{early, central, lateZip, bounds} = easyMode <|> hardMode
        where
        easyMode = (\lateZip -> z{lateZip}) <$> moveCarriage dir lateZip
        hardMode = case (dir, central) of
            (Forwards, _) -> Other GoHyper
            (Backwards, _ :<<|| _) -> One $ CentralMlZ
                { early
                , centralZip = toZipper Backwards central
                , late = fromZipper lateZip
                , bounds
                }
            (Backwards, Nil) -> One $ EarlyMlZ
                { earlyZip = toZipper dir early
                , central
                , late = fromZipper lateZip
                , bounds
                }

    push dir (C c) z = case z of
        EarlyMlZ{earlyZip} -> z{earlyZip = push dir c earlyZip}
        CentralMlZ{centralZip} -> z{centralZip = push dir (C c) centralZip}
        LateMlZ{lateZip} -> z{lateZip = push dir c lateZip}
    push Forwards (Lb lineInfo) EarlyMlZ{earlyZip, central, late, bounds} = EarlyMlZ
        { earlyZip = LZ.LZ
            { before
            , here = toZipper Backwards pre
            , after = Empty
            , h2o = H2O (lineInfo, Dirty) True
            , bounds = Other Backwards
            }
        , central = L post (markDirty h2o) :<<|| central
        , late
        , bounds
        }
        where LZ.LZ{before, here = TZ pre post, h2o} = earlyZip
    push Backwards (Lb lineInfo) EarlyMlZ{earlyZip, central, late, bounds} = CentralMlZ
        { early = LS (before :|> StartSel pre) (H2O (lineInfo, Dirty) True)
        , centralZip = toZipper Forwards $ L post (markDirty h2o) :<<|| central
        , late
        , bounds
        }
        where LZ.LZ{before, here = TZ pre post, after, h2o} = earlyZip
    push dir lb@(Lb _) z@CentralMlZ{centralZip} = z{centralZip = push dir lb centralZip}
    push Forwards (Lb lineInfo) LateMlZ{early, central, lateZip, bounds} = CentralMlZ
        { early
        , centralZip = toZipper Backwards $ central :||>> L pre (H2O (lineInfo, Dirty) True)
        , late = LS (EndSel post :<| after) (markDirty h2o)
        , bounds
        }
        where LZ.LZ{before, here = TZ pre post, after, h2o} = lateZip
    push Backwards (Lb lineInfo) LateMlZ{early, central, lateZip, bounds} = LateMlZ
        { early
        , central = central :||>> L pre (H2O (lineInfo, Dirty) True)
        , lateZip = LZ.LZ
            { before = Empty
            , here = toZipper Forwards post
            , after
            , h2o = markDirty h2o
            , bounds = Other Forwards
            }
        , bounds
        }
        where LZ.LZ{here = TZ pre post, after, h2o} = lateZip



