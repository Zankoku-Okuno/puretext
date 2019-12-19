module PureText.Lines.Core where

import PureText.Util

import Data.String(IsString(..))
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T


--  TODO is there a way to specialize this for `a ~ ()`, since it could then be a newtype?
data LineHydration a = H2O
    { lineInfo :: a
    , hasLinebreak :: !Bool
    }
    deriving(Functor)
instance Semigroup a => Semigroup (LineHydration a) where
    (H2O x _) <> (H2O y lb) = H2O (x <> y) lb
instance Monoid a => Monoid (LineHydration a) where
    mempty = H2O mempty False


data Line a = L
    { unL :: Text
    , h2o :: LineHydration a
    }
    deriving(Functor)

pattern NoLine :: (Monoid a) => Line a
pattern NoLine <- L "" (H2O _ False)
    where NoLine = L "" mempty

pattern (:<||) :: Monoid a => Char -> Line a -> Line a
pattern c :<|| l <- (unconsChar -> Just (c, l))
    where (:<||) = consChar

pattern (:||>) :: Monoid a => Line a -> Char -> Line a
pattern l :||> c <- (unsnocChar -> Just (l, c))
    where (:||>) = snocChar

pattern Linebreak :: Monoid a => Line a -> Line a
pattern Linebreak l <- (stripLinebreak -> Just l)
    where Linebreak (L l h2o) = L l h2o{hasLinebreak = True}

pattern NoLinebreak :: Monoid a => Line a -> Line a
pattern NoLinebreak l <- l@(L _ (H2O _ False))
    where NoLinebreak (L l h2o) = L l h2o{hasLinebreak = False}

pattern (:~||) :: Monoid a => a -> Line a -> Line a
pattern info :~|| l <- l@(L _ (H2O info _))
    where info :~|| L l h2o = L l h2o{lineInfo = info}


data Lines a = Ls
    { initLines :: Seq (Text, a)
    , lastLine :: Line a
    }

pattern Nil :: (Monoid a) => Lines a
pattern Nil = Ls { initLines = Empty, lastLine = NoLine }

pattern (:<<||) :: Monoid a => Line a -> Lines a -> Lines a
pattern l :<<|| ls <- (unconsLines -> Just (l, ls))
    where (:<<||) = consLines

pattern (:||>>) :: Monoid a => Lines a -> Line a -> Lines a
pattern ls :||>> l <- (unsnocLines -> Just (ls, l))
    where (:||>>) = snocLines

instance Semigroup a => Semigroup (Lines a) where
    Ls{initLines = xs, lastLine = L t (H2O info True)} <> ys@Ls{..} =
        ys{initLines = (xs :|> (t, info)) <> initLines}
    Ls{initLines = xs, lastLine = L t (H2O _ False)} <> Ls{initLines = Empty, lastLine = L t' h2o} =
        Ls{initLines = xs, lastLine = L (t <> t') h2o}
    Ls{initLines = xs, lastLine = L t (H2O _ False)} <> ys@Ls{initLines = (t', info') :<| ls} =
        ys{initLines = xs <> ((t <> t', info') :<| ls)}
instance Monoid a => Monoid (Lines a) where
    mempty = Nil

------------ Helper Functions ------------

consLines :: Monoid a => Line a -> Lines a -> Lines a
consLines l Nil = Ls{initLines = Empty, lastLine = l}
consLines (L t (H2O info True)) xs@Ls{initLines} = xs{initLines = (t, info) :<| initLines}
consLines (L t (H2O info False)) xs@Ls{initLines = Empty, lastLine = L t' (H2O info' tlb)} = xs{lastLine = L (t <> t') (H2O (info <> info') tlb)}
consLines (L t (H2O info False)) xs@Ls{initLines = (t', info') :<| ls} = xs{initLines = (t <> t', info <> info') :<| ls}

unconsLines :: Monoid a => Lines a -> Maybe (Line a, Lines a)
unconsLines Nil = Nothing
unconsLines Ls{initLines = Empty, lastLine} = Just (lastLine, Nil)
unconsLines xs@Ls{initLines = (t, info) :<| ls} = Just (L t (H2O info True), xs{initLines = ls})

snocLines :: Monoid a => Lines a -> Line a -> Lines a
snocLines Nil l = Ls{initLines = Empty, lastLine = l}
snocLines xs@Ls{initLines = ls, lastLine = L t (H2O info True)} l' = xs{initLines = ls :|> (t, info), lastLine = l'}
snocLines xs@Ls{lastLine = L t (H2O info False)} (L t' (H2O info' tlb)) = xs{lastLine = L (t <> t') (H2O (info <> info') tlb)}

unsnocLines :: Monoid a => Lines a -> Maybe (Lines a, Line a)
unsnocLines Nil = Nothing
unsnocLines xs@Ls{initLines = Empty, lastLine} = Just (Nil, lastLine)
unsnocLines xs@Ls{initLines = ls :|> (t, info), lastLine} = Just (xs{initLines = ls, lastLine = L t (H2O info True)}, lastLine)


consChar :: Monoid a => Char -> Line a -> Line a
consChar c (L t h2o) = L (T.cons c t) h2o

unconsChar :: Monoid a => Line a -> Maybe (Char, Line a)
unconsChar (L "" _) = Nothing
unconsChar (L (c :< t) h2o) = Just (c, L t h2o)

snocChar :: Monoid a => Line a -> Char -> Line a
snocChar (L t h2o) c = L (T.snoc t c) h2o

unsnocChar :: Monoid a => Line a -> Maybe (Line a, Char)
unsnocChar (L "" _) = Nothing
unsnocChar (L (t :> c) h2o) = Just (L t h2o, c)


stripLinebreak :: Monoid a => Line a -> Maybe (Line a)
stripLinebreak (NoLinebreak _) = Nothing
stripLinebreak (L t h2o) = Just $ L t h2o{hasLinebreak = False}
