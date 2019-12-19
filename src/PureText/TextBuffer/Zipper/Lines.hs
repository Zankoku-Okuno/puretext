module PureText.TextBuffer.Zipper.Lines
    ( LinesZipper
    ) where

import PureText.TextBuffer.Zipper.Base
import PureText.TextBuffer.Zipper.Slice
import qualified PureText.TextBuffer.Zipper.Slice.Buffer as BS
import PureText.Zipper.Text
import PureText.Zipper.LineSlices
import PureText.TextBuffer.Lines
import PureText.TextBuffer.Lines.Core
import PureText.Util

import Data.Functor
import Control.Applicative

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq


data LinesZipper a = LsZ
    { above :: Lines (a, Dirt)
    , here :: TextZipper
    , h2o :: LineHydration (a, Dirt)
    , below :: Lines (a, Dirt)
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
