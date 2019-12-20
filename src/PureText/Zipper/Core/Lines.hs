{- |
This is a zipper over 'Lines', centering on a 'TextZipper'.
It is possibly the most reusable of the zippers in "PureText.Zipper",
    since it does not impose any notion of slicing lines or making selections,
    instead opting to be a simple two-dimensional zipper over lines of text.
Nevertheless, it is only currently used by "PureText.Zipper.HyperLine" at the moment.

This module contains the core data structures and algorithms for
    making single-point edits within a line-oriented text buffer.
It is not meant to be imported directly, but instead used through the
    much smaller "PureText.Zipper.Lines" interface.
It is provided only in case frontends use this
    as a data structure when there is only a single selection,
    in which case the internal data structure may need to be read to be rendered.
-}
module PureText.Zipper.Core.Lines where

import PureText.Zipper.Base
import PureText.Slice.Core
import PureText.Zipper.Text
import PureText.Zipper.LineSlices
import PureText.Lines
import PureText.Lines.Core
import PureText.Util

import Data.Functor
import Control.Applicative

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

{- |
A two-dimensional zipper over line-oriented text.

This zipper also keeps track of which lines are 'Dirty',
    which indicates
        that charaters were 'push'ed or 'pop'ed from the line,
        that the line was split, or
        a linebreak was removed, forming a longer line.
When converting back to 'Lines', it is simple enough to
    'fmap fst' to remove the 'Dirt' markings if they are unwanted.
-}
data LinesZipper a = LsZ
    { above :: Lines (a, Dirt)
    -- ^ the lines above the current line
    , here :: TextZipper
    -- ^ a zipper into the current line
    , h2o :: LineHydration (a, Dirt)
    -- ^ whether the line has a trailing linebreak,
    --   whether the line is 'Dirty',
    --   and any additional user information about the line (possibly invalidated if the line is dirty)
    , below :: Lines (a, Dirt)
    -- ^ the lines below the current line
    }

instance (Monoid a) => Zippy (LinesZipper a) where
    type Base (LinesZipper a) = Lines (a, Dirt)
    type Blocked (LinesZipper a) = Maybe
    type Elem (LinesZipper a) = Charbreak a

    toZipper dir Nil = error "precondition violation: attempt to convert empty lines to a LinesZipper"
    toZipper Forwards (L t h2o :<<|| below) = LsZ
        { above = Nil
        , here = toZipper Forwards t
        , below
        , h2o
        }
    toZipper Backwards (above :||>> L t h2o) = LsZ
        { above
        , here = toZipper Backwards t
        , below = Nil
        , h2o
        }

    fromZipper LsZ{above, here, below, h2o} = above <> hereLines <> below
        where
        hereLines = OneLine $ L (fromZipper here) h2o

    moveCarriage dir z@LsZ{..} = easyMode <|> hardMode
        where
        easyMode = (\here -> z{here}) <$> moveCarriage dir here
        hardMode = case (dir, above, below) of
            (Forwards, _, L t h2o' :<<|| below) -> Just LsZ
                { above = above :||>> L (fromZipper here) h2o
                , here = toZipper dir t
                , below
                , h2o = h2o'
                }
            (Forwards, _, Nil) -> Nothing
            (Backwards, above :||>> L t h2o', _) -> Just LsZ
                { above
                , here = toZipper dir t
                , below = L (fromZipper here) h2o :<<|| below
                , h2o = h2o'
                }
            (Backwards, Nil, _) -> Nothing

    push dir (C c) z@LsZ{here, h2o} = z{here = push dir c here, h2o = markDirty h2o}
    push Forwards (Lb lineInfo) LsZ{above, here, below, h2o} = LsZ
        { above = above
        , here = toZipper Backwards pre
        , below = L post (markDirty h2o) :<<|| below
        , h2o = H2O (lineInfo, Dirty) True
        }
        where (pre, post) = splitTextZipper here
    push Backwards (Lb lineInfo) LsZ{above, here, below, h2o} = LsZ
        { above = above :||>> L pre (H2O (lineInfo, Dirty) True)
        , here = toZipper Forwards post
        , below
        , h2o = markDirty h2o
        }
        where (pre, post) = splitTextZipper here
