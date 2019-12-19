module PureText.TextBuffer.Lines
    ( Lines(Nil, (:<<||), (:||>>))
    , LinebreakSequence, fromText, toText
    , Line, LineHydration(..)
    , Charbreak(..)
    , pattern NoLine, pattern EmptyLine, pattern OneLine
    , hasTrailingLinebreak
    ) where

import PureText.TextBuffer.Lines.Core
import PureText.Util

import Data.Foldable
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T


------------ To/From Text ------------

type LinebreakSequence = Text

{-
    If an empty LinebreakSequence is passed, the resulting string will be a single line containing all the Text with no linebreaks.
-}
fromText :: Monoid a => LinebreakSequence -> Text -> Lines a
-- TODO: hopefully this has good performance
--      possibly I could speed it up for single-char LinebreakSequences by using fold
fromText lb "" = Nil
fromText lb str = loop Nil NoLine str
    where
    loop !acc !l "" = acc :||>> l
    loop !acc !l (T.stripPrefix lb -> Just t) = loop (acc :||>> Linebreak l) NoLine t
    loop !acc !l (c :< t) = loop acc (l :||> c) t

toText :: Monoid a => LinebreakSequence -> Lines a -> Text
toText lb Ls{..} = combine $ fmap fst initLines :|> unL lastLine
    where
    combine = if (hasLinebreak . h2o) lastLine
        then T.concat . fmap (<> lb) . toList
        else T.intercalate lb . toList


------------ Charbreak ------------

data Charbreak a = C Char | Lb a


------------ Tiny Utilities ------------

hasTrailingLinebreak :: Lines a -> Bool
hasTrailingLinebreak = hasLinebreak . h2o . lastLine

pattern EmptyLine :: Monoid a => Line a
pattern EmptyLine = Linebreak NoLine

pattern OneLine :: Monoid a => Line a -> Lines a
pattern OneLine l = Ls{initLines = Empty, lastLine = l}
