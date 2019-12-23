{-
classes:
    HasElems
    HasIndices

    HasElems -> ListCore
    ListCore -> SeqCore

    ListCore -> Foldable

    ListCore, Foldable -> ListLike
    SeqCore, Foldable -> SeqLike

instances:

    ListCore -> deriving SeqCore via SlowSeq

    via AListCore: ListCore -> deriving Foldable
    via ASeqCore:  SeqCore -> deriving Foldable

    via AListCore: ListCore -> ListLike
    via ASeqCore:  ListLike, SeqCore -> ListLike


-}
module PureText.ListLike.Base where

import Prelude (($!), Integral(..))
import qualified Prelude
import Data.Function

import Data.Bool
import Data.Int
import Prelude(Num(..))
import Data.Eq

import Data.Maybe
import Data.Tuple
import Data.Monoid
import Data.Functor
import Control.Arrow
import Control.Applicative
import Control.Monad


pattern Nil :: ListCore f => f
pattern Nil <- (null -> True)
    where Nil = nil

pattern (:<) :: ListCore f => Elem f -> f -> f
pattern x :< xs <- (uncons -> Just (x, xs))
    where (:<) = cons

pattern (:>) :: SeqCore f => f -> Elem f -> f
pattern xs :> x <- (unsnoc -> Just (xs, x))
    where (:>) = snoc



class HasElems f where
    type Elem f :: *

class HasIndices f where
    type SizeT f :: *
type HasIndexes f = HasIndices f

{- |
The classic defining axioms of lists are 'nil', 'cons', and 'uncons',
    which are all constant-time.

Some types have sub-linear 'append' (e.g. finger trees),
    so we've included 'append' as an additional introduction form.
Filling out the primitive operations centered around @append@ are
    'nil', 'singleton', and 'uncons'.

Some other functions, with constant-time default implementations are also given here.
For additional list-like algorithms,
    see 'ListFold' (general linear-time fold-based algorithms),
    and 'ListLike' (additional algorithms commonly expected on lists).
Also see 'SeqCore' and the coresponding 'SeqFold', 'SeqLike'
    for list-like operations that work on the back of the list.
-}
class HasElems f => ListCore f where
    {-# MINIMAL nil, (cons | (singleton, append)), uncons #-}

    nil :: f

    cons :: Elem f -> f -> f
    cons x xs = singleton x `append` xs
    singleton :: Elem f -> f
    singleton x = x :< Nil
    append :: f -> f -> f
    Nil `append` ys = ys
    (x :< xs) `append` ys = x :< (xs `append` ys)

    uncons :: f -> Maybe (Elem f, f)

    null :: f -> Bool
    null = maybe True (const False) . uncons

class ListCore f => SeqCore f where
    snoc :: f -> Elem f -> f
    snoc xs x = xs `append` singleton x
    unsnoc :: f -> Maybe (f, Elem f)

{- | Adapter newtype for use in deriving-via.
Its 'SeqCore' implementation lifts 'cons' and 'uncons' into 'snoc' and 'unsnoc'
    in the naïve, linear-time way.

Only use this in desperation.
-}
newtype SlowSeq a = SlowSeq { unSlowSeq :: a }
    deriving(HasElems, ListCore)
instance ListCore f => SeqCore (SlowSeq f) where
    snoc Nil y = singleton y
    snoc (x :< xs) y = x :< (snoc xs y)
    unsnoc xs = case uncons xs of
        Just (y, Nil) -> Just (Nil, y)
        Just (x, xs) -> let Just (xs, y) = unsnoc xs in Just (x :< xs, y)
        Nothing -> Nothing

{- |
Folds are the ur-operation for lists.
Maps and filters are defineable by fold,
    and from those come all the other algorithms you can expect.

Aside from just folds, this class also defines fold-based algorithms
    that consume all the data.
This is as opposed to algorithms that, though they could be fold based,
    consume data from the front (see 'ListLike'),
    consume data from the back (see 'SeqLike'), or
    consume data from the middle (see 'ArrayLike').
-}
class ListCore f => Foldable f where
    {-# MINIMAL foldr, foldl #-}

    foldMap :: Monoid m => (Elem f -> m) -> f -> m
    foldMap f = foldr (mappend . f) mempty
    fold :: Monoid (Elem f) => f -> Elem f
    fold = foldMap id

    foldr :: (Elem f -> b -> b) -> b -> f -> b
    foldl :: (b -> Elem f -> b) -> b -> f -> b

    foldr' :: (Elem f -> b -> b) -> b -> f -> b
    foldr' f z0 xs = foldl f' id xs z0
        where f' k x z = k $! f x z
    foldl' :: (b -> Elem f -> b) -> b -> f -> b
    foldl' f z0 xs = foldr f' id xs z0
        where f' x k z = k $! f z x

    foldr1 :: (Elem f -> Elem f -> Elem f) -> f -> Elem f
    foldr1 f xs = fromMaybe err $ foldr mf Nothing xs
        where
        mf x m = Just $ maybe x (x `f`) m
        err = Prelude.errorWithoutStackTrace "foldr1: empty structure"
    foldl1 :: (Elem f -> Elem f -> Elem f) -> f -> Elem f
    foldl1 f xs = fromMaybe err $ foldl mf Nothing xs
        where
        mf m y = Just $ maybe y (`f` y) m
        err = Prelude.errorWithoutStackTrace "foldl1: empty structure"

    -- mapping
    map :: ListCore f' => (Elem f -> Elem f') -> f -> f'
    map f = foldr ((:<) . f) Nil
    rigidMap :: (Elem f -> Elem f) -> f -> f
    rigidMap = map

    -- filtering
    filter :: (Elem f -> Bool) -> f -> f
    filter p =  foldr f Nil
        where f x = if p x then (x :<) else id
    exclude :: (Elem f -> Bool) -> f -> f
    exclude p = filter (not . p)
    partition :: (Elem f -> Bool) -> f -> (f, f)
    partition p = foldr f (Nil, Nil)
        where f x = if p x then first (x :<) else second (x :<)

    -- conversion
    toList :: f -> [Elem f]
    toList = foldr (:) []

    fromList :: [Elem f] -> f
    fromList [] = Nil
    fromList (x:xs) = x :< fromList xs


newtype AListCore a = AListCore { unListCore :: a }
    deriving(HasElems, ListCore)
instance ListCore f => Foldable (AListCore f) where
    foldr f z Nil = z
    foldr f z (x :< xs) = x `f` foldr f z xs
    foldl f z Nil = z
    foldl f z (x :< xs) = foldl f (z `f` x) xs

newtype ASeqCore a = ASeqCore { unSeqCore :: a }
    deriving(HasElems, ListCore, SeqCore)
instance SeqCore f => Foldable (ASeqCore f) where
    foldr f z (ASeqCore xs) = foldr f z (AListCore xs)
    foldl f z Nil = z
    foldl f z (xs :> x) = foldl f z xs `f` x


{- this lacks some default implementations; see the ListLike instance of AListCore for basic ones, or derive via some other way -}
{- I may alternately hyst give the list-optimized ones as defaults, then advise using via ASeqCore if they aren't good -}
class (ListCore f, Foldable f) => ListLike f where
    {-# MINIMAL (splitAt | (take, drop)), (span | (takeWhile, dropWhile)) #-}

    -- length-based operations
    length :: Num n => f -> n
    length = foldl' (\l _ -> l+1) 0

    splitAt :: Integral n => n -> f -> (f, f)
    splitAt n xs = (take n xs, drop n xs)
    take :: Integral n => n -> f -> f
    take n = fst . splitAt n
    drop :: Integral n => n -> f -> f
    drop n = snd . splitAt n

    -- predicate the prefix
    span :: (Elem f -> Bool) -> f -> (f, f)
    span p xs = (takeWhile p xs, dropWhile p xs)
    takeWhile :: (Elem f -> Bool) -> f -> f
    takeWhile p = fst . span p
    dropWhile :: (Elem f -> Bool) -> f -> f
    dropWhile p = snd . span p
    break :: (Elem f -> Bool) -> f -> (f, f)
    break p = span (not . p)

    -- match the prefix
    stripPrefix :: Eq (Elem f) => f -> f -> Maybe f
    stripPrefix Nil postfix = Just postfix
    stripPrefix _ Nil = Nothing
    stripPrefix (x :< needle) (y :< haystack)
        | x == y = stripPrefix needle haystack
        | otherwise = Nothing

    isPrefixOf :: Eq (Elem f) => f -> f -> Bool
    isPrefixOf needle = isJust . stripPrefix needle

    -- misc
    tails :: f -> [f] -- FIXME can't I use a ListLike f isntead of [f]?
    tails Nil = []
    tails l@(x :< xs) = l : tails xs


instance ListCore f => ListLike (AListCore f) where
    take 0 xs = Nil
    take n Nil = Nil
    take n (x :< xs) = x :< take (n - 1) xs

    drop 0 xs = xs
    drop n Nil = Nil
    drop n (_ :< xs) = drop (n - 1) xs

    takeWhile p Nil = Nil
    takeWhile p (x :< xs)
        | p x = x :< takeWhile p xs
        | otherwise = Nil

    dropWhile p Nil = Nil
    dropWhile p l@(x :< xs)
        | p x = l
        | otherwise = dropWhile p xs


instance (ListLike f, SeqCore f) => ListLike (ASeqCore f) where
    splitAt n0 = go n0 Nil
        where
        go 0 !acc xs = (acc, xs)
        go n !acc Nil = (acc, Nil)
        go n !acc (x :< xs) = go (n - 1) (acc :> x) xs

    span p = go Nil
        where
        go !acc Nil = (acc, Nil)
        go !acc (x :< xs)
            | p x = go (acc :> x) xs
            | otherwise = (acc, x :< xs)


class (ListLike f, SeqCore f) => SeqLike f where
    -- length-based operations
    splitAtEnd :: Int -> f -> (f, f)
    splitAtEnd n0 = flip (go n0) Nil
        where
        go 0 xs !acc = (xs, acc)
        go n Nil !acc = (Nil, acc)
        go n (xs :> x) !acc = go (n - 1) xs (x :< acc)
    takeEnd :: Int -> f -> f
    takeEnd n = snd . splitAtEnd n
    dropEnd :: Int -> f -> f
    dropEnd n = fst . splitAtEnd n

    -- predicate the suffix
    spanEnd :: (Elem f -> Bool) -> f -> (f, f)
    spanEnd p = flip go Nil
        where
        go Nil !acc = (Nil, acc)
        go (xs :> x) !acc
            | p x = go xs (x :< acc)
            | otherwise = (xs :> x, acc)
    takeWhileEnd :: (Elem f -> Bool) -> f -> f
    takeWhileEnd p = snd . spanEnd p
    dropWhileEnd :: (Elem f -> Bool) -> f -> f
    dropWhileEnd p = fst . spanEnd p
    breakEnd :: (Elem f -> Bool) -> f -> (f, f)
    breakEnd p = spanEnd (not . p)

    -- match the suffix
    stripSuffix :: Eq (Elem f) => f -> f -> Maybe f
    stripSuffix Nil prefix = Just prefix
    stripSuffix _ Nil = Nothing
    stripSuffix (needle :> x) (haystack :> y)
        | x == y = stripSuffix needle haystack
        | otherwise = Nothing

    isSuffixOf :: Eq (Elem f) => f -> f -> Bool
    isSuffixOf needle = isJust . stripSuffix needle

    -- misc
    inits :: f -> [f]
    inits = go Nil
        where
        go acc Nil = []
        go xs (y :< ys) = xs : go (xs :> y) ys

    reverse :: f -> f
    reverse Nil = Nil
    reverse (xs :> x) = x :< reverse xs



-- instance (SeqCore f) => SeqLike (SlowSeq f) where -- TODO?

{-
{- FIXME: this is a strict array,
but probly it'd be ok to have a generous array (with take/drop as usual)
and perhaps it'd be useful to have an array with an index type whose bounds are known-checked (if I can manage it)
-}
class (SeqLike f, HasIndices f) => ArrayLike f where
    {-# MINIMAL splitAt | (drop, take) #-}

    index :: f -> SizeT f -> Maybe (Elem f)
    index xs i = do
        (xs, ys) <- splitAt i xs
        fst <$> uncons ys

    insertAt :: f -> SizeT f -> Elem f -> Maybe f
    insertAt xys i x = do
        (xs, ys) <- splitAt i xys
        Just $ xs `append` cons x ys

    deleteAt :: f -> SizeT f -> Maybe f
    deleteAt xys i = do
        (xs, ys) <- splitAt i xys
        append xs . snd <$> uncons ys

    {- index-oriented
    elemIndex :: Eq item => item -> full -> Maybe Int
    elemIndices :: (Eq item, ListLike result Int) => item -> full -> result
    findIndex :: (item -> Bool) -> full -> Maybe Int
    findIndices :: ListLike result Int => (item -> Bool) -> full -> result
    -}

(!?) :: ArrayLike f => f -> SizeT f -> Maybe (Elem f)
(!?) = index
-}

    









-- SetLike

























    -- sum :: Num a => t a -> a
    -- product :: Num a => t a -> a

    -- elem :: Eq a => a -> t a -> Bool infix 4
    -- maximum :: forall a. Ord a => t a -> a
    -- minimum :: forall a. Ord a => t a -> a


    -- basic functional algorithms
    -- TODO? seems just like a fold over append
    {-
    concat :: ListLike f' f => f' -> f
    -- concatMap :: ListLike f' Elem f' => ???
    rigidConcatMap :: (Elem f -> f) -> f -> f
    -}

{-









    stripInfix :: Eq (Elem f) => f -> f -> Maybe (f, f)
    stripInfix needle Nil = Nothing
    stripInfix needle haystack = case stripPrefix needle haystack of
        Just post -> Just (Nil, post)
        Nothing -> do
            (x, xs) <- uncons haystack
            (pre, post) <- stripInfix needle xs
            Just (x :< pre, post)

    isInfixOf :: Eq (Elem f) => f -> f -> Bool
    isInfixOf needle Nil = False
    isInfixOf needle haystack@(_ :< xs)
        =  isPrefixOf needle haystack
        || isInfixOf needle xs



    -- splitOn :: e -> f -> t f
    -- splitOnInfix :: f -> f -> t f

    {- search for elements
    elem :: Eq item => item -> full -> Bool
    notElem :: Eq item => item -> full -> Bool
    find :: (item -> Bool) -> full -> Maybe item
    -}







-- suggested O(n) default override for splitAt method for SeqLike types
-- the default from ListLike traverses the list twice
-- obvs, if a sub-linear algorithm is available, use that instead
seqlikeSplitAt :: SeqLike f => Int -> f -> (f, f)
seqlikeSplitAt n0 = go n0 Nil
    where
    go 0 !acc xs = (acc, xs)
    go n !acc Nil = (acc, Nil)
    go n !acc (x :< xs) = go (n - 1) (acc :> x) xs

seqlikeStripInfix :: (SeqLike f, Eq (Elem f)) => f -> f -> Maybe (f, f)
seqlikeStripInfix = go Nil
    where
    go !pre needle haystack@(x :< xs) = case stripPrefix needle haystack of
        Nothing -> go (pre :> x) needle xs
        Just post -> Just (pre, post)
    go !pre needle Nil = Nothing






    -- specific algorithms
    {-
    intersperse :: e -> f -> f
    replicate
    -- any, all, minimum, maximum
    -}
    -- TODO
    {- monadic
    sequence :: (Monad m, ListLike fullinp (m item)) => fullinp -> m full
    mapM :: (Monad m, ListLike full' item') => (item -> m item') -> full -> m full'
    rigidMapM :: Monad m => (item -> m item) -> full -> m full
    -}
    {- genericize Int
    genericLength :: Num a => full -> a
    genericTake :: Integral a => a -> full -> full
    genericDrop :: Integral a => a -> full -> full
    genericSplitAt :: Integral a => a -> full -> (full, full)
    genericReplicate :: Integral a => a -> item -> full 
    -}
    {- sorting and grouping
    group :: (ListLike full' full, Eq item) => full -> full'
    sort :: Ord item => full -> full
    groupBy :: (ListLike full' full, Eq item) => (item -> item -> Bool) -> full -> full'
    sortBy :: (item -> item -> Ordering) -> full -> full
    -}





    {- set-like algorithms (SetLike? with slow versions over ListLike)
    nub :: Eq item => full -> full
    delete :: Eq item => item -> full -> full
    deleteFirsts :: Eq item => full -> full -> full
    union :: Eq item => full -> full -> full
    intersect :: Eq item => full -> full -> full
    insert :: Ord item => item -> full -> full
    nubBy :: (item -> item -> Bool) -> full -> full
    deleteBy :: (item -> item -> Bool) -> item -> full -> full
    deleteFirstsBy :: (item -> item -> Bool) -> full -> full -> full
    unionBy :: (item -> item -> Bool) -> full -> full -> full
    intersectBy :: (item -> item -> Bool) -> full -> full -> full
    insertBy :: (item -> item -> Ordering) -> item -> full -> full
    -}

-}