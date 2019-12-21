module PureText.Prelude
    ( module X
    , clamp, minBy, maxBy
    , both
    , Neither(..)
    , maybeToOne, maybeToOther
    ) where

import Prelude as X (Integer, Num(..), ($!))
import Data.Int as X
import Data.Word as X
import Data.Bool as X
import Data.Char as X
import Data.String as X (String, IsString(..))
import Data.Text as X (Text)
import Text.Read as X hiding ((+++))
import Text.Show as X
import Data.Eq as X
import Data.Ord as X

import Data.Tuple as X

import Data.Maybe as X
import Data.Either as X
import Data.Foldable as X hiding (null, length)
import PureText.ListLike as X
import Data.Sequence as X (Seq(..))

import Data.Function as X
import Data.Semigroup as X hiding (First(..), Last(..))
import Data.Monoid as X

import Data.Functor as X
import Control.Arrow as X
import Control.Applicative as X
import Control.Monad as X hiding (mapM, sequence, sequence_)


------------ Basic Numerics ------------

clamp :: (Ord n) => (n, n) -> n -> n
clamp (low, high) = max low . min high

minBy :: (Ord b) => (a -> b) -> a -> a -> a
minBy f a b = case compare (f a) (f b) of
    GT -> b
    _ -> a
maxBy :: (Ord b) => (a -> b) -> a -> a -> a
maxBy f a b = case compare (f a) (f b) of
    LT -> b
    _ -> a


------------ Missing Tuple ------------

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)


------------ Neither ------------

data Neither b a
    = One a
    | Other b
    | Neither
    deriving(Functor)

instance Applicative (Neither b) where
    pure = One
    One f <*> One x = One (f x)
    Other f <*> _ = Other f
    Neither <*> _ = Neither

instance Monad (Neither b) where
    return = pure
    (One x) >>= k = k x
    (Other x) >>= _ = Other x
    Neither >>= _ = Neither

instance Alternative (Neither b) where
    empty = Neither
    One a <|> _ = One a
    Other _ <|> One a = One a
    Other _ <|> Other a = Other a
    Other a <|> Neither = Other a
    Neither <|> a = a

maybeToOne :: Maybe a -> Neither b a
maybeToOne Nothing = Neither
maybeToOne (Just a) = One a

maybeToOther :: Maybe b -> Neither b a
maybeToOther Nothing = Neither
maybeToOther (Just b) = Other b
