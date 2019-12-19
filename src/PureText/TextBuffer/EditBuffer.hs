-- {-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module PureText.TextBuffer.EditBuffer where

import PureText.TextBuffer.Zipper.Base
import PureText.TextBuffer.Zipper.Slice
import PureText.Zipper.LineSlices
import PureText.Zipper.HyperLine
import qualified PureText.TextBuffer.Zipper.Slice.Buffer as B
import PureText.Zipper.Text
import PureText.TextBuffer.Lines
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




data EditZipper a
    = LineZ
        { over :: BufferSlices a
        , here :: LineSlicesZipper a
        , under :: BufferSlices a
        }
    | HyperZ
        { over :: BufferSlices a
        , near :: HyperLineZipper a
        , under :: BufferSlices a
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
        Nothing -> ctor (editEmpty_ dir) mempty
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
            where (next, under') = hyperJump dir (here, under)
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
            where (next, over') = hyperJump dir (here, over)
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


sublight :: Monoid a => Direction -> BufferSlice a -> LineSlicesZipper a
sublight dir = toZipper dir . toLineSlices_

editEmpty_ :: Monoid a => Direction -> LineSlicesZipper a
editEmpty_ dir = toZipper dir $ sliceLine NoLine

toLineSlices_ :: Monoid a => BufferSlice a -> LineSlices (a, Dirt)
toLineSlices_ (Whole dirt (OneLine l)) = sliceLine $ l <&> (,dirt)
toLineSlices_ (Cutup l) = l
toLineSlices_ _ = error "precondition violation"

hyperJump :: Monoid a => Direction -> (LineSlicesZipper a, BufferSlices a) -> (HyperLine a, BufferSlices a)
hyperJump Forwards (here, under) = case B.hyperUncons $ prepForHyperline here <> under of
    Just it -> it
    Nothing -> error "postcondition violation: went to HyperLine prematurely"
hyperJump Backwards (here, over) = case B.hyperUnsnoc $ over <> prepForHyperline here of
    Just it -> swap it
    Nothing -> error "postcondition violation: went to HyperLine prematurely"
