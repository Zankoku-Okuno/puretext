{- |
When a selection crosses multiple lines,
    it is useful to have fingers to the start\/end of the line
    as well as to the start\/end of the selection.
See "PureText.Zipper.Edit" for why we need a zipper inside a 'HyperLine'.

This module contains the core data structures and algorithms for
    making single-point edits within a hyperline.
It is not meant to be imported directly, but instead used through the
    much smaller "PureText.Zipper.HyperLine" interface.
It is provided only in case frontends use this
    as a data structure when there is only a single selection,
    in which case the internal data structure may need to be read to be rendered.
-}
module PureText.Zipper.Core.HyperLine where

import PureText.Zipper.Base
import PureText.Slice.Core
import PureText.Zipper.Text
import PureText.Zipper.LineSlices
import PureText.Zipper.Lines
import PureText.Lines
import PureText.Lines.Core
import PureText.Util

import Data.Functor
import Control.Applicative

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

{- |
A zipper over multiple lines, centered on a zipper over the current line.

This data structure gives us fingers to several places:

    * the start and end of the current line,
    * the start and end of the selection,
    * and incidentally, we also get fingers to the start of the first line touched by the selection,
        and the end of the last line touched by the selection.
        See the edge cases below to clarify \"touching\".

There are several edge cases, which nonetheless have clear semantics:

  * When the central 'Lines' are 'Nil', that indicates
        the selection spans only two lines.
        I.e. all selected text is contained in the 'StartSel' and 'EndSel' of the first\/last line respectively
        (excepting the linebreak between the two, of course, which is stored outside the line characters).
  * When the 'StartSel' in the first line is empty, that indicates
        the selection includes only the linebreak from that first line.
  * Similarly, when the 'EndSel' in the last line is empty, that indicates
        the selection includes the trailing linebreak of the last central line
        (or of the first line if there are no central lines).
  * Conversely, when the first\/last line contains only a 'StartSel' or 'EndSel' (resp.) 'LineSlice',
        that indicates the line has all of its characters selected
        (excepting the trailing linebreak for the last line; see the edge case where the 'EndSel' is empty above).

-}
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
    push dir (Lb lineInfo) EarlyMlZ{earlyZip, central, late, bounds} = case dir of
        Forwards -> EarlyMlZ
            { earlyZip = pre
            , central = post :<<|| central
            , late
            , bounds
            }
        Backwards -> CentralMlZ
            { early = fromZipper pre
            , centralZip = toZipper Forwards $ post :<<|| central
            , late
            , bounds
            }
        where (pre, post) = splitEarlyZipper lineInfo earlyZip
    push dir lb@(Lb _) z@CentralMlZ{centralZip} = z{centralZip = push dir lb centralZip}
    push dir (Lb lineInfo) LateMlZ{early, central, lateZip, bounds} = case dir of
        Forwards -> CentralMlZ
            { early
            , centralZip = toZipper Backwards $ central :||>> pre
            , late = fromZipper post
            , bounds
            }
        Backwards -> LateMlZ
            { early
            , central = central :||>> pre
            , lateZip = post
            , bounds
            }
        where (pre, post) = splitLateZipper lineInfo lateZip

    pop dir z@EarlyMlZ{earlyZip, central} = easyMode <|> hardMode
        where
        easyMode = (\(c, earlyZip) -> (C c, z{earlyZip})) <$> pop dir earlyZip
        hardMode = case (dir, central) of
            -- popping the linebreak
            (Forwards, l :<<|| central') -> One (Lb lineInfo, z{earlyZip = earlyZip', central = central'})
                where (lineInfo, earlyZip') = mergeEarlyZipper earlyZip l
            -- needs to exit hyperspace
            (Forwards, Nil) -> Other GoHyper
            -- must have run into the start of StartSel
            (Backwards, _) -> Neither
    -- TODO pop central
    pop dir z@CentralMlZ{early, centralZip, late, bounds} = easyMode <|> hardMode
        where
        easyMode = maybeToOne $ (\(c, centralZip) -> (c, z{centralZip})) <$> pop dir centralZip
        hardMode = case (dir, fromZipper centralZip) of
            (Forwards, central' :||>> l) -> One
                ( Lb lineInfo
                , LateMlZ{early, central = central', lateZip, bounds}
                )
                where (lineInfo, lateZip) = mergeLateZipper l (hyperZipper Forwards late)
            (Backwards, l :<<|| central') -> One
                ( Lb lineInfo
                , EarlyMlZ{earlyZip, central = central', late, bounds}
                )
                where (lineInfo, earlyZip) = mergeEarlyZipper (hyperZipper Backwards early) l
            _ -> error "invariant violation: CenterMlZ with empty center detected"
    pop dir z@LateMlZ{central, lateZip} = easyMode <|> hardMode
        where
        easyMode = (\(c, lateZip) -> (C c, z{lateZip})) <$> pop dir lateZip
        hardMode = case (dir, central) of
            -- popping the linebreak
            (Backwards, central' :||>> l) -> One (Lb lineInfo, z{central = central', lateZip = lateZip'})
                where (lineInfo, lateZip') = mergeLateZipper l lateZip
            -- needs to exit hyperspace
            (Backwards, Nil) -> Other GoHyper
            -- must have run into the end of EndSel
            (Forwards, _) -> Neither
