{-# LANGUAGE UndecidableInstances #-}
{- The idea here is to have line fragments, never the entire line -}
module PureText.Select.LineCell
    ( LineCells, LineCell(..)
    ) where

import PureText.Prelude
import Prelude (error) -- TODO consider removing uses of error where feasible

import PureText.Select.Base
import PureText.Select.Text

import PureText.Holding
import PureText.ListLike.AppendBased




newtype LineCells = LC { unLC :: Seq LineCell }
    deriving ListLike via (AsAppendBasedList LineCells)
    deriving SeqLike via (AsAppendBasedList LineCells)
data LineCell
    = Un Textish
    | Inl Textish Bounds
    | Early Textish Bounds
    | Central Textish
    | Late Textish


instance Semigroup LineCells where
    xs <> (LC Empty) = xs
    (LC Empty) <> ys = ys
    (LC (xs :|> x)) <> (LC (y :<| ys)) = case (x, y) of
        (Un "", _) -> LC xs <> LC (y :<| ys)
        (_, Un "") -> LC (xs :|> x) <> LC ys
        (Un x, Un y) -> merge Un x y
        -- (Early x b, Early y) -> merge (flip Early b) x y
        (Early x b, Late y) -> merge (flip Inl b) x y
        (Early x b, Central y) -> merge (flip Early b) x y
        (Central x, Central y) -> merge Central x y
        (Central x, Late y) -> merge Late x y
        -- (Late x, Late y) -> merge Late x y
        (Early _ _, _) -> error "invariant violation: bad append after a start selection cell"
        (Central _, _) -> error "invariant violation: bad append inside center of multi-line selection cell"
        (_, Central _) -> error "invariant violation: bad append inside center of multi-line selection cell"
        (_, Late _) -> error "invariant violation: bad prepend before an end selection cell"
        where
        merge ctor x y = LC $ xs <> singleton (Un $ x <> y) <> ys
instance Monoid LineCells where
    mempty = LC mempty


instance AppendBasedList LineCells where
    newtype AppendBasedElem LineCells = LCElem { unLCElem :: LineCell }
    appendBasedSingleton = LC . singleton . unLCElem
    appendBasedCons (LCElem x) (LC xs) = LC (x :< xs)
    appendBasedUncons xs = case uncons (unLC xs) of
        Just (x, xs) -> Just (LCElem x, LC xs)
        Nothing -> Nothing
    -- TODO it'd be nice to have a HoldingAppendBased sort of thing for stuff like:
    --     span p = both LC . span p . unLC
    --     takeWhile p = LC . takeWhile p . unLC
    --     dropWhile p = LC . dropWhile p . unLC
    --     break p = both LC . break p . unLC
instance AppendBasedSeq LineCells where
    appendBasedSnoc (LC xs) (LCElem x) = LC (xs :> x)
    appendBasedUnsnoc xs = case unsnoc (unLC xs) of
        Just (xs, x) -> Just (LC xs, LCElem x)
        Nothing -> Nothing


-- instance ListLike LineCells where
--     type Elem LineCells = LineCell
--     nil = mempty
--     cons x = (singleton x <>)
--     uncons = (second LC <$>) . uncons . unLC
--     singleton = LC . singleton
--     append = (<>)
--     splitAt n = both LC . splitAt n . unLC
--     take n = LC . take n . unLC
--     drop n = LC . drop n . unLC
--     -- map :: ListLike f' => (Elem f -> Elem f') -> f -> f'
--     -- rigidMap :: (Elem f -> Elem f) -> f -> f
--     null = null . unLC
--     length = length . unLC
--     -- filter :: (Elem f -> Bool) -> f -> f
--     -- exclude :: (Elem f -> Bool) -> f -> f
--     -- partition :: (Elem f -> Bool) -> f -> (f, f)
--     span p = both LC . span p . unLC
--     takeWhile p = LC . takeWhile p . unLC
--     dropWhile p = LC . dropWhile p . unLC
--     break p = both LC . break p . unLC
--     stripPrefix (LC needle) = fmap LC . stripPrefix needle . unLC
--     isPrefixOf (LC needle) = isPrefixOf needle . unLC

-- instance SeqLike LineCells where
--     snoc xs = (xs <>) . singleton
--     unsnoc = (first LC <$>) . unsnoc . unLC
--     splitAtEnd n = both LC . splitAtEnd n . unLC
--     takeEnd n = LC . takeEnd n . unLC
--     dropEnd n = LC . dropEnd n . unLC
--     spanEnd p = both LC . spanEnd p . unLC
--     takeWhileEnd p = LC . takeWhileEnd p . unLC
--     dropWhileEnd p = LC . dropWhileEnd p . unLC
--     breakEnd p = both LC . breakEnd p . unLC
--     stripSuffix (LC needle) = fmap LC . stripSuffix needle . unLC
--     isSuffixOf (LC needle) = isSuffixOf needle . unLC



-- instance ListLike (AsChars LineCells) Char where
-- instance ListLike (AsCharish (LineCells a)) Charish where -- TODO
