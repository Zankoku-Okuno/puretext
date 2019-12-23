{-#LANGUAGE TupleSections, PatternSynonyms, ViewPatterns, RecordWildCards, BangPatterns #-}
module PureText.FuzzyComplete.Engine
    ( FuzzyCompletions
        , emptyFuzzyCompletions, toFuzzyCompletions
        , addFuzzyCompletion, removeFuzzyCompletion
        , addFuzzyCompletions, removeFuzzyCompletions
    , fuzzySearch, fuzzyChoice
    ) where

import PureText.Prelude

import qualified Data.List as List

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


------ Searching and Choosing ------

fuzzySearch :: FuzzyCompletions -> Text -> [Text]
fuzzySearch = usingCache basicSearch

fuzzyChoice :: (Text, Text) -> FuzzyCompletions -> FuzzyCompletions
fuzzyChoice (key, !choice) (St db) = St $ Map.adjust go key db
    where
    go (_, size) = (Just choice, size)


basicSearch :: FuzzyDb -> Text -> [Text]
basicSearch db input = sortInCostGroups (countPrefixSkips input) (Map.keys db)
    -- FIXME add matches that are case-insensitive
-- hammingSearch -- TODO where things are ordered by a modified Hamming distance that allows strings of deletions to be weight 1
    -- essentially, I'd calculate the modified hamming distance for each, then sort, but if the distance rises over a threshold, then ignore


------ Maintaining the Database ------

type FuzzyDb = Map Text (Maybe Text, Word) -- TODO use less boxing/lifting?

newtype FuzzyCompletions = St FuzzyDb

emptyFuzzyCompletions :: FuzzyCompletions
emptyFuzzyCompletions = St Map.empty

toFuzzyCompletions :: (Foldable t, Elem t ~ Text) => t -> FuzzyCompletions
toFuzzyCompletions new = St $ foldl' (flip bagAdd) Map.empty new


addFuzzyCompletion :: Text -> FuzzyCompletions -> FuzzyCompletions
addFuzzyCompletion new (St db) = St $ bagAdd new db
removeFuzzyCompletion :: Text -> FuzzyCompletions -> FuzzyCompletions
removeFuzzyCompletion new (St db) = St $ bagRemove new db

addFuzzyCompletions new (St db) = St $ foldl' (flip bagAdd) db new
removeFuzzyCompletions new (St db) = St $ foldl' (flip bagRemove) db new

-- TODO setFuzzyCompletions, keeping old last selections
setFuzzyCompletions :: (Foldable t, Elem t ~ Text) => t -> FuzzyCompletions -> FuzzyCompletions
setFuzzyCompletions new (St db) = St $ Map.mapWithKey copyLast fresh
    where
    St fresh = toFuzzyCompletions new
    copyLast k (_, count) = (, count) $! (fst =<< Map.lookup k db)


------ Cost Metrics ------

-- FIXME use a tuple cost metric: first is as here, second is if there are end chars remaining
-- as opposed to counting global skips, which adds one on transition to the skip state
countPrefixSkips :: Text -> Text -> Maybe Int
countPrefixSkips expect input = consume expect input
    where
    -- FIXME I guarantee there are performance improvements to be gained here
    consume "" _ = Just 0
    consume _ "" = Nothing
    consume (c:<cs) (c':<cs')
        | c == c' = consume cs cs'
        | otherwise = skip (c:<cs) cs'
    skip "" _ = Just 0
    skip _ "" = Nothing
    skip (c:<cs) (c':<cs')
        | c == c' = (1+) <$> consume cs cs'
        | otherwise = skip (c:<cs) cs'


------ Helpers ------

usingCache :: (FuzzyDb -> Text -> [Text]) -> FuzzyCompletions -> Text -> [Text]
usingCache f (St db) input = case cacheResult of
    Nothing -> dbResult
    Just last -> [last] <> List.delete last dbResult
    where
    cacheResult = Map.lookup input db >>= fst
    dbResult = f db input

sortInCostGroups :: (Foldable t, Elem t ~ a, Ord a) => (a -> Maybe Int) -> t -> [a]
sortInCostGroups f xs = fmap snd $ List.sort $ foldMap f' xs
    where
    f' x = maybe [] (adapt x) $ f x
    adapt x = (:[]) . (,x)


bagAdd :: Text -> FuzzyDb -> FuzzyDb
bagAdd = Map.alter theAlter
    where
    theAlter Nothing = Just (Nothing, 1)
    theAlter (Just (last, n)) = Just $! (last, n + 1)
bagRemove :: Text -> FuzzyDb -> FuzzyDb
bagRemove = Map.alter theAlter
    where
    theAlter Nothing = Nothing
    theAlter (Just (_, 1)) = Nothing
    theAlter (Just (last, n)) = Just $! (last, n - 1)
