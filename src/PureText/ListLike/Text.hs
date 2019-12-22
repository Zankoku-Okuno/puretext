module PureText.ListLike.Text where

import PureText.ListLike.Base
import Prelude (Num(..))
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

instance ListLike Text where
    type Elem Text = Char
    nil = T.empty
    cons = T.cons
    uncons = T.uncons
    singleton = T.singleton
    append = T.append
    splitAt = T.splitAt
    take = T.take
    drop = T.drop
    -- map :: ListLike f' => (Elem f -> Elem f') -> f -> f'
    rigidMap = T.map
    null = T.null
    length = T.length
    filter = T.filter
    -- exclude :: (Elem f -> Bool) -> f -> f
    partition = T.partition
    span = T.span
    takeWhile = T.takeWhile
    dropWhile = T.dropWhile
    break = T.break
    stripPrefix = T.stripPrefix
    isPrefixOf = T.isPrefixOf
    stripInfix = seqlikeStripInfix
    isInfixOf = T.isInfixOf

instance SeqLike Text where
    snoc = T.snoc
    unsnoc = T.unsnoc
    splitAtEnd n xs = T.splitAt (length xs - n) xs
    takeEnd = T.takeEnd
    dropEnd = T.dropEnd
    spanEnd p xs = (dropWhileEnd p xs, takeWhileEnd p xs)
    takeWhileEnd = T.takeWhileEnd
    dropWhileEnd = T.dropWhileEnd
    -- breakEnd :: (Elem f -> Bool) -> f -> (f, f)
    stripSuffix = T.stripSuffix
    isSuffixOf = T.isSuffixOf
