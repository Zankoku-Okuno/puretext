-- {-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{- |
Even though the user might make multiple selections,
    all modifications to a text buffer occur only at one place.
This is because a modification might adjust the boundaries of selections.
Edits on multiple selections will have to be done by
    looping a single-point editing function over each selection.

This module contains the core data structures and algorithms for
    making single-point edits within a text buffer.
It is not meant to be imported directly, but instead used through the
    much smaller "PureText.Zipper.Edit" interface.
It is provided only in case frontends use this
    as a data structure when there is only a single selection,
    in which case the internal data structure may need to be read to be rendered.
-}
module PureText.Zipper.Core.Edit where

import PureText.Zipper.Base
import PureText.Slice.Core
import PureText.Zipper.LineSlices
import PureText.Zipper.HyperLine
import qualified PureText.Slice.Buffer as B
import PureText.Zipper.Text
import PureText.Lines
import PureText.Util

import Data.Functor
import Control.Applicative
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T



-- I think it's better to have peek, move, delete, and insert as the primitives
-- my physics brain tells me that relativity means moving the carriage is the same as moving stuff across it
-- however, my progarmming brain knows that push/pop across has very different semantics if they're going to take/give characters


-- {push,pop}{Line,Char}EditZipper{Forwards,Backwards}<Landmark>{Upto,Through}Selection
-- crossSelection{Forwards,Backwards}
-- mergeSelections i.e. be outside of a selection, but have selections both left and right
-- newSelection i.e. be outside a selection and put boundaries left and right


{- |
The basic idea is to have a zipper centered on the current line,
    which is itself a zipper over the text of that line.
In addition, it would be good to finger
    (i.e. allow O(1) access while staying pure-functional)
    the start\/end of a line, as well as the start\/end of a selection if one is available.
The problem is that a selection may span multiple lines.

'EditZipper' therefore has two cases which follow the hyperspace analogy laid out in (FIXME where am I going to describe this analogy?).

INVARIANT: The @over@ field ends with 'MlSel' if and only if
    the @here@\/@near@ field starts in a 'EndSel'.

INVARIANT: The @under@ field starts with 'MlSel' if and only if
    the @here@\/@near@ field ends in a 'StartSel'.

WARNING: The invariants of 'BufferSlices' are allowed to be violated
    in the first element of @under@ and the last element of @over@,
    just as long as the analogous 'EditZipper' invariants are maintained.
-}
{- FIXME if LineSlices , BufferSlice, and BufferSlices had two new type arguments (kind = Sublight EnteringHyperspace, ExitingHyperspace)
    then I could encode these invariants in the type system.
-}
data EditZipper a
    {- |
    In this case, any selection the zipper carriage might be in is less than one line long,
        and so we need only center the zipper on a single line.
    It is the basic case, rather than the complicated one.
    -}
    = LineZ
        { over :: BufferSlices a -- ^ lines above the current selection
        , here :: LineSlicesZipper a -- ^ the line the carriage is on, possibly in an inline selection
        , under :: BufferSlices a -- ^ lines below the current selection
        }
    {- |
    This is the multi-line selection case.
    The zipper is centered on a 'HyperLineZipper', which is a multi-line zipper
        that contains all the lines the selection touches
        (even if it only touches the linebreaks).
    -}
    | HyperZ
        { over :: BufferSlices a -- ^ lines above the current selection
        , near :: HyperLineZipper a -- ^ the hyperline the carriage is in
        , under :: BufferSlices a -- ^ lines below the current selection
        }

{- WARNING: there are a bunch of places where I'm not checking pre/postconditions and invariants early

Importantly, you can only Enter hyperspace from LineZ, and exit from HyperZ; that suggests some refactoring.
Also, there's a big undocumented difference between hyperNear and toZipper:
    use hypernear when creating a HyperZ, toZipper elsewise
-}

instance (Monoid a) => Zippy (EditZipper a) where
    type Base (EditZipper a) = BufferSlices a
    type Blocked (EditZipper a) = Maybe
    type Elem (EditZipper a) = Charbreak a

    toZipper dir buf = case dtor buf of
        Just (l, ls) -> ctor (sublight dir l) ls
        Nothing -> ctor (toZipper dir $ sliceLine NoLine) mempty
        where
        (ctor, dtor) = case dir of
            Forwards -> (\here under -> LineZ{over = mempty, here, under}, B.uncons)
            Backwards -> (\here over -> LineZ{over, here, under = mempty}, (swap <$>) . B.unsnoc)

    fromZipper LineZ{over, here, under} = over <> adapt (fromZipper here) <> under
        where adapt = B.singleton . B.fromLineSlices
    fromZipper HyperZ{over, near, under} = over <> adapt (fromZipper near) <> under
        where
        adapt (early, central, late) = miniadapt early <> B.singleton central <> miniadapt late
        miniadapt = B.singleton . B.fromLineSlices

    moveCarriage dir z@LineZ{over, here, under} = case (dir, moveCarriage dir here) of
        (_, One here) -> Just z{here}
        (Forwards, Neither) -> do
            (next, under') <- B.uncons under
            pure LineZ
                { over = B.snoc over (B.fromLineSlices $ fromZipper here)
                , here = sublight dir next
                , under = under'
                }
        (Forwards, Other GoHyper) -> pure HyperZ
            { over
            , near = toZipper dir next
            , under = under'
            }
            where (next, under') = hyperjump dir (here, under)
        (Backwards, Neither) -> do
            (over', next) <- B.unsnoc over
            pure LineZ
                { over = over'
                , here = sublight dir next
                , under = B.cons (B.fromLineSlices $ fromZipper here) under
                }
        (Backwards, Other GoHyper) -> pure HyperZ
            { over = over'
            , near = toZipper dir next
            , under
            }
            where (next, over') = hyperjump dir (here, over)
    moveCarriage dir z@HyperZ{over, near, under} = case (dir, moveCarriage dir near) of
        (_, One near) -> Just z{near}
        (Forwards, Other GoHyper) -> pure LineZ
            { over = B.hyperSnoc over (early, central)
            , here = toZipper dir late
            , under
            }
            where
            (early, central, late) = fromZipper near
        (Backwards, Other GoHyper) -> pure LineZ
            { over
            , here = toZipper dir early
            , under = B.hyperCons (central, late) under
            }
            where
            (early, central, late) = fromZipper near
        (_, Neither) -> error "postcondition violation: failed to exit HyperLine"

    push dir (C c) z@LineZ{here} = z{here = push dir c here}
    push Forwards (Lb lineInfo) LineZ{over, here, under} = case bounds of
        -- FIXME the ColCursor should be reset (also for moveCarriage)
        Nothing -> LineZ
            { over
            , here = toZipper Backwards pre
            , under = B.fromLineSlices post `B.cons` under
            }
        Just bounds -> HyperZ
            { over
            , near = splitInlSel Forwards bounds (pre, post)
            , under
            }
        where (pre, post, bounds) = splitLineSlicesZipper lineInfo here
    push Backwards (Lb lineInfo) LineZ{over, here, under} = case bounds of
        Nothing -> LineZ
            { over = over `B.snoc` B.fromLineSlices pre
            , here = toZipper Forwards post
            , under
            }
        Just bounds -> HyperZ
            { over
            , near = splitInlSel Backwards bounds (pre, post)
            , under
            }
        where (pre, post, bounds) = splitLineSlicesZipper lineInfo here
    push dir c z@HyperZ{near} = z{near = push dir c near}

    pop dir z@LineZ{over, here, under} = case pop dir here of
        One (c, here) -> Just (C c, z{here})
        Other _ -> Nothing -- WARNING I'm abusing the Other branch in LineSlicesZipper.pop to say that it hit a selection boundary, not necessarily multi-line
        Neither -> case (dir, B.unsnoc over, B.uncons under) of
            (Forwards, _, Just (Cutup l, under')) -> Just
                (Lb lineInfo, LineZ
                    { over
                    , here = mergeLineSlicesZipper dir slices $ toZipper dir l
                    , under = under'
                    })
                where LS{slices, h2o = H2O (lineInfo, _) _} = fromZipper here
            (Backwards, Just (over', Cutup l), _) -> Just
                (Lb lineInfo, LineZ
                    { over = over'
                    , here = mergeLineSlicesZipper dir slices $ toZipper dir l
                    , under
                    })
                where LS{slices, h2o = H2O (lineInfo, _) _} = fromZipper here
    -- TODO HyperZ

{-| Helper function to center 'EditZipper' on a single-line buffer slice.
The name follows the hyperspace analogy laid out in (FIXME where am I going to describe this analogy?).

PRECONDITION: the passed 'BufferSlice' must be single-line (i.e. not 'MlSel').
 -}
sublight :: Monoid a => Direction -> BufferSlice a -> LineSlicesZipper a
sublight dir = toZipper dir . toLineSlices_
    where
    toLineSlices_ (Whole dirt (OneLine l)) = sliceLine $ l <&> (,dirt)
    toLineSlices_ (Cutup l) = l
    toLineSlices_ _ = error "precondition violation"

{-| Helper to center the 'EditZipper' on a multi-line selection.
The name follows the hyperspace analogy laid out in (FIXME where am I going to describe this analogy?).

The idea is to transform the 'LineSlicesZipper' back into a 'BufferSlice'
    so that it can be put back onto the 'BufferSlices' above or below the current (single-line) focus.
Then, a 'HyperLine' will be split off those (reconstructed) 'BufferSlices' and returned
    along with the remaining 'BufferSlices'.

PRECONDITION: the passed 'LineSlicesZipper' must either:
    a) end with a 'StartSel' if 'Direction' is 'Forwards', or
    b) start with an 'EndSel' if 'Direction' is 'Backwards'.

WARNING: This relies on the postcondition for 'prepForHyperline', which is that the resulting
    'BufferSlices' is a 'Cutup' that ends\/starts with 'StartSel' or 'EndSel' respectively
    if its input did.

WARNING: This relies on the invariant that an 'EndSel' is always preceded by an 'MlSel',
    that 'StartSel' is always followed by 'MlSel',
    and that 'EditZipper' maintains the corresponding invariants when it splits 'BufferSlices' apart.
-}
hyperjump :: Monoid a => Direction
          -- ^ which direction the carriage is moving in order to enter a hyperline
          -> (LineSlicesZipper a, BufferSlices a)
          -- ^ outer line (ending\/starting in 'StartSel' or 'EndSel' if direction is 'Forwards' or 'Backwards' resp.)
          -- and additional slices (from below/above resp.) with a 'MlSel' on the appropriate edge
          -> (HyperLine a, BufferSlices a)
          -- ^ the 'HyperLine' holding the next multi-line selection,
          -- along with further 'BufferSlices' not in the selection
hyperjump Forwards (here, under) = case B.hyperUncons $ prepForHyperline here <> under of
    Just it -> it
    Nothing -> error "precondition violation: went to HyperLine prematurely"
hyperjump Backwards (here, over) = case B.hyperUnsnoc $ over <> prepForHyperline here of
    Just it -> swap it
    Nothing -> error "precondition violation: went to HyperLine prematurely"
