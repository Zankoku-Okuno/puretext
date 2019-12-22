module PureText.ListLike.Base where

import Data.Function

import Data.Bool
import Data.Int
import Prelude(Num(..))
import Data.Eq


import Data.Maybe
import Data.Tuple

pattern Nil :: ListLike f => f
pattern Nil <- (null -> True)
    where Nil = nil

pattern (:<) :: ListLike f => Elem f -> f -> f
pattern x :< xs <- (uncons -> Just (x, xs))
    where (:<) = cons

pattern (:>) :: SeqLike f => f -> Elem f -> f
pattern xs :> x <- (unsnoc -> Just (xs, x))
    where (:>) = snoc


{- FIXME a lot of the listlike methods are more like foldable

perhaps we should have a module of foldable functions, and a module of list functions
they may overlap naming and semantically, but then it'll be possible to choose which one
    dependeing on which algorithm is right for the datatype
-}
class ListLike f where

    {-# MINIMAL nil, (cons | (singleton, append)), uncons #-}

    type Elem f :: *
    -- usually: uncons (cons x xs) === Just (c, xs)
    --      but c should contain x in some way
    -- LAW: uncons nil === Nothing
    -- LAW: if Monoid f, mempty === nil

    -- minimal definition: nil, cons, uncons


    -- atomic intro/elim forms
    nil :: f
    cons :: Elem f -> f -> f
    cons x = append (singleton x)
    uncons :: f -> Maybe (Elem f, f)


    -- larger intro/elim forms
    singleton :: Elem f -> f
    singleton x = x :< Nil

    append :: f -> f -> f
    Nil `append` ys = ys
    (x :< xs) `append` ys = x :< (xs `append` ys)

    splitAt :: Int -> f -> (f, f)
    splitAt n xs = (take n xs, drop n xs)
    take :: Int -> f -> f
    take 0 xs = Nil
    take n Nil = Nil
    take n (x :< xs) = x :< take (n - 1) xs
    drop :: Int -> f -> f
    drop 0 xs = xs
    drop n Nil = Nil
    drop n (x :< xs) = drop (n - 1) xs


    -- basic functional algorithms
    map :: ListLike f' => (Elem f -> Elem f') -> f -> f'
    map f Nil = Nil
    map f (x :< xs) = f x :< map f xs
    rigidMap :: (Elem f -> Elem f) -> f -> f
    rigidMap = map
    -- TODO? seems just like a fold over append
    {-
    concat :: ListLike f' f => f' -> f
    -- concatMap :: ListLike f' Elem f' => ???
    rigidConcatMap :: (Elem f -> f) -> f -> f
    -}


    null :: f -> Bool
    null = maybe True (const False) . uncons

    length :: f -> Int
    length = go 0
        where
        go !acc Nil = acc
        go !acc (_ :< xs) = go (acc + 1) xs


    -- predication
    filter :: (Elem f -> Bool) -> f -> f
    filter p Nil = Nil
    filter p (x :< xs) = if p x then (x :< xs') else xs'
        where xs' = filter p xs
    exclude :: (Elem f -> Bool) -> f -> f
    exclude p = filter (not . p)
    partition :: (Elem f -> Bool) -> f -> (f, f)
    partition p xs = (filter p xs, exclude p xs)

    span :: (Elem f -> Bool) -> f -> (f, f)
    span p xs = (takeWhile p xs, dropWhile p xs)
    takeWhile :: (Elem f -> Bool) -> f -> f
    takeWhile p Nil = Nil
    takeWhile p (x :< xs)
        | p x = x :< takeWhile p xs
        | otherwise = Nil
    dropWhile :: (Elem f -> Bool) -> f -> f
    dropWhile p Nil = Nil
    dropWhile p (x :< xs)
        | p x = dropWhile p xs
        | otherwise = x :< xs
    break :: (Elem f -> Bool) -> f -> (f, f)
    break p = span (not . p)


    -- detect sublists
    stripPrefix :: Eq (Elem f) => f -> f -> Maybe f
    stripPrefix Nil postfix = Just postfix
    stripPrefix _ Nil = Nothing
    stripPrefix (x :< needle) (y :< haystack)
        | x == y = stripPrefix needle haystack
        | otherwise = Nothing

    isPrefixOf :: Eq (Elem f) => f -> f -> Bool
    isPrefixOf Nil _ = True
    isPrefixOf _ Nil = False
    isPrefixOf (x :< needle) (y :< haystack)
        | x == y = isPrefixOf needle haystack
        | otherwise = False

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

    -- tails


    -- splitOn :: e -> f -> t f
    -- splitOnInfix :: f -> f -> t f

    {- search for elements
    elem :: Eq item => item -> full -> Bool
    notElem :: Eq item => item -> full -> Bool
    find :: (item -> Bool) -> full -> Maybe item
    -}
    {- index-oriented
    index :: full -> Int -> item
    elemIndex :: Eq item => item -> full -> Maybe Int
    elemIndices :: (Eq item, ListLike result Int) => item -> full -> result
    findIndex :: (item -> Bool) -> full -> Maybe Int
    findIndices :: ListLike result Int => (item -> Bool) -> full -> result
    insertAt, deleteAt
    -}

    {- conversion
    toList :: full -> [item]
    fromList :: [item] -> full
    fromListLike :: ListLike full' item => full -> full'
    -}


-- TODO end-oriented functions
{-
slowSnoc
slowUnsnoc
slowSplitAtEnd, slowTakeEnd, slowDropEnd,
slowSpanEnd, slowTakeWhileEnd, slowDropWhileEnd, slowBreakEnd
slowIsSuffixOf :: Eq item => full -> full -> Bool
slowStripSuffix :: Eq item => full -> full -> Maybe full
slowInits
-}

class ListLike f => SeqLike f where
    {-# MINIMAL unsnoc #-}

    snoc :: f -> Elem f -> f
    snoc xs = (append xs) . singleton
    unsnoc :: f -> Maybe (f, Elem f)

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

    -- detect sublists
    stripSuffix :: Eq (Elem f) => f -> f -> Maybe f
    stripSuffix Nil prefix = Just prefix
    stripSuffix _ Nil = Nothing
    stripSuffix (needle :> x) (haystack :> y)
        | x == y = stripSuffix needle haystack
        | otherwise = Nothing

    isSuffixOf :: Eq (Elem f) => f -> f -> Bool
    isSuffixOf Nil _ = True
    isSuffixOf _ Nil = False
    isSuffixOf (needle :> x) (haystack :> y)
        | x == y = isSuffixOf needle haystack
        | otherwise = False

    -- inits

-- suggested O(n) default override for splitAt method for SeqLike types
-- the default from ListLike traverses the list twice
-- obvs, if a sub-linear algorithm is available, use that instead
seqlikeSplitAt :: SeqLike f => Int -> f -> (f, f)
seqlikeSplitAt n0 = go n0 Nil
    where
    go 0 !acc xs = (acc, xs)
    go n !acc Nil = (acc, Nil)
    go n !acc (x :< xs) = go (n - 1) (acc :> x) xs

-- see seqlikeSplitAt, which exists for the same reason
seqlikePartition :: SeqLike f => (Elem f -> Bool) -> f -> (f, f)
seqlikePartition p = go (Nil, Nil)
    where
    go (!y, !n) Nil = (y, n)
    go (!y, !n) (x :< xs)
        | p x = go (y :> x, n) xs
        | otherwise = go (y, n :> x) xs

-- see seqlikeSplitAt, which exists for the same reason
seqlikeSpan :: SeqLike f => (Elem f -> Bool) -> f -> (f, f)
seqlikeSpan p = go Nil
    where
    go !acc Nil = (acc, Nil)
    go !acc (x :< xs)
        | p x = go (acc :> x) xs
        | otherwise = (acc, x :< xs)

seqlikeStripInfix :: (SeqLike f, Eq (Elem f)) => f -> f -> Maybe (f, f)
seqlikeStripInfix = go Nil
    where
    go !pre needle haystack@(x :< xs) = case stripPrefix needle haystack of
        Nothing -> go (pre :> x) needle xs
        Just post -> Just (pre, post)
    go !pre needle Nil = Nothing






    -- specific algorithms
    {-
    reverse :: f -> f
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
