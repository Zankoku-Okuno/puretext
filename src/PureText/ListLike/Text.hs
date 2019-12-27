module PureText.ListLike.Text where

import PureText.ListLike.Base
import Prelude (Num(..), fromIntegral)
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Exts as GHC

import Data.Function


instance HasElems Text where
    type Elem Text = Char

instance ListCore Text where
    nil = T.empty
    cons = T.cons
    uncons = T.uncons
    singleton = T.singleton
    append = T.append
    null = T.null
instance SeqCore Text where
    snoc = T.snoc
    unsnoc = T.unsnoc

instance Foldable Text where
    foldr = T.foldr
    foldl = T.foldl
    foldl' = T.foldl'
    foldr1 = T.foldr1
    foldl1 = T.foldl1
    rigidMap = T.map
    filter = T.filter
    partition = T.partition
    toList = GHC.toList
    fromList = GHC.fromList

instance ListLike Text where
    length = fromIntegral . T.length
    splitAt n = T.splitAt (fromIntegral n)
    take n = T.take (fromIntegral n)
    drop n = T.drop (fromIntegral n)
    span = T.span
    takeWhile = T.takeWhile
    dropWhile = T.dropWhile
    break = T.break
    stripPrefix = T.stripPrefix
    isPrefixOf = T.isPrefixOf
    -- stripInfix = seqlikeStripInfix
    -- isInfixOf = T.isInfixOf
instance SeqLike Text where
    splitAtEnd n xs = T.splitAt (length xs - (fromIntegral n)) xs
    takeEnd = T.takeEnd
    dropEnd = T.dropEnd
    spanEnd p xs = (dropWhileEnd p xs, takeWhileEnd p xs)
    takeWhileEnd = T.takeWhileEnd
    dropWhileEnd = T.dropWhileEnd
    stripSuffix = T.stripSuffix
    isSuffixOf = T.isSuffixOf
