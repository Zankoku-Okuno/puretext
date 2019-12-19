module PureText.Util where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T

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


------------ Text Patterns ------------

pattern (:<) :: Char -> Text -> Text
pattern c :< t <- (T.uncons -> Just (c, t))
    where (:<) = T.cons

pattern (:>) :: Text -> Char -> Text
pattern t :> c <- (T.unsnoc -> Just (t, c))
    where (:>) = T.snoc


------------ Tuple Stuff ------------

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)


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
