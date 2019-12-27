module PureText.Select.Cell where
    -- FIXME
    -- ( Cells, Cell(..)
    -- , Select(..)
    -- -- , MergeCellsLeft(..)
    -- -- , MergeCellsRight(..)
    -- ) where

import Prelude (error)
import PureText.Prelude
import PureText.Select.Base
import PureText.Select.Text

data Select = Selected | Unselected

data Cell (sel :: Select) a where
    UnSel :: Textish -> Cell Unselected a -- WARNING the text should never be empty
    -- | LineInfo a
    -- | Linebreak
    InlSel1 :: Textish -> Bounds -> Cell Selected a -- WARNING: this one is only here to serve as the single element of an InlSel
    -- | EarlySel Textish Bounds
    -- | CentralSel Textish
    -- | LateSel Textish


data Cells (sel :: Select) a where
    UnSels :: Seq (Cell Unselected a) -> Cells Unselected a
    PointSel :: Cells Selected a
    InlSel :: Textish -> Bounds -> Cells Selected a -- WARNING the text should never be empty
    -- MlSel :: Seq (Cell Selected a) -> Cells Selected a


instance HasElems (Cells sel a) where
    type Elem (Cells sel a) = Cell sel a

instance ListCore (Cells Unselected a) where
    nil = UnSels $ nil
    singleton = UnSels . singleton
    append = (<>)
    uncons (UnSels xs) = second UnSels <$> uncons xs
instance SeqCore (Cells Unselected a) where
    unsnoc (UnSels xs) = first UnSels <$> unsnoc xs

instance ListCore (Cells Selected a) where
    nil = PointSel
    singleton (InlSel1 t b) = InlSel t b
    append = (<>)
    uncons PointSel = Nothing
    uncons (InlSel t b) = Just (InlSel1 t b, Nil)
instance SeqCore (Cells Selected a) where
    unsnoc PointSel = Nothing
    unsnoc (InlSel t b) = Just (Nil, InlSel1 t b)

deriving via ASeqCore (Cells Unselected a) instance Foldable (Cells Unselected a)
deriving via ASeqCore (Cells Unselected a) instance ListLike (Cells Unselected a)
instance SeqLike (Cells Unselected a) where

deriving via ASeqCore (Cells Selected a) instance Foldable (Cells Selected a)
deriving via ASeqCore (Cells Selected a) instance ListLike (Cells Selected a)
instance SeqLike (Cells Selected a) where

instance Semigroup (Cells Unselected a) where
    Nil <> ys = ys
    xs <> Nil = xs
    (UnSels (before :> x)) <> (UnSels (y :< after)) = UnSels $ before <> center <> after
        where
        center = case (x, y) of
            -- WARNING: These two are commented b/c there should never be an empty UnSel
            -- (UnSel "", y) -> singleton y
            -- (x, UnSel "") -> singleton x
            (UnSel t, UnSel t') -> singleton $ UnSel (t <> t')
            (x, y) -> singleton x <> singleton y
instance Monoid (Cells Unselected a) where
    mempty = nil

instance Semigroup (Cells Selected a) where
    Nil <> ys = ys
    xs <> Nil = xs
    (InlSel t b) <> (InlSel t' b') = InlSel (t <> t') b -- FIXME how to merge the bounds properly?
instance Monoid (Cells Selected a) where
    mempty = nil


select :: Bounds -> Cells Unselected a -> Cells Selected a
select b (UnSels Nil) = PointSel
select b (UnSels (UnSel t :< Nil)) = InlSel t b

deselect :: Cells Selected a -> Cells Unselected a
deselect PointSel = Nil
deselect (InlSel t _) = singleton $ UnSel t

alterBounds :: (Bounds -> Bounds) -> Cells Selected a -> Cells Selected a
alterBounds _ PointSel = PointSel
alterBounds f (InlSel t b) = InlSel t (f b)

cellBounds :: Cells Selected a -> Bounds
cellBounds PointSel = Cursor
cellBounds (InlSel _ bounds) = bounds

{-
newtype MergeCellsLeft a = MergeL { unMergeL :: Cells a }
newtype MergeCellsRight a = MergeR { unMergeR :: Cells a }

instance HasElems (MergeCellsLeft a) where type Elem (MergeCellsLeft a) = Cell a
instance HasElems (MergeCellsRight a) where type Elem (MergeCellsRight a) = Cell a

instance ListCore (MergeCellsLeft a) where
    nil = MergeL nil
    singleton = MergeL . singleton
    uncons = (second MergeL <$>) . uncons . unMergeL

    Nil `append` ys = ys
    xs `append` Nil = xs
    (MergeL (Cells (before :> x))) `append` (MergeL (Cells (y :< after))) = MergeL . Cells $ before <> center <> after
        where
        center = case (x, y) of
            (UnSel t, UnSel t') -> singleton $ UnSel (t <> t')
            (UnSel t, InlSel t' bounds) -> singleton $ UnSel (t <> t')
            (InlSel t bounds, UnSel t') -> singleton $ InlSel (t <> t') bounds
            (InlSel t bounds, InlSel t' _) -> singleton $ InlSel (t <> t') bounds
instance ListCore (MergeCellsRight a) where
    nil = MergeR nil
    singleton = MergeR . singleton
    uncons = (second MergeR <$>) . uncons . unMergeR

    Nil `append` ys = ys
    xs `append` Nil = xs
    (MergeR (Cells (before :> x))) `append` (MergeR (Cells (y :< after))) = MergeR . Cells $ before <> center <> after
        where
        center = case (x, y) of
            (UnSel t, UnSel t') -> singleton $ UnSel (t <> t')
            (InlSel t _, UnSel t') -> singleton $ UnSel (t <> t')
            (UnSel t, InlSel t' bounds) -> singleton $ InlSel (t <> t') bounds
            (InlSel t _, InlSel t' bounds) -> singleton $ InlSel (t <> t') bounds

instance SeqCore (MergeCellsLeft a) where
    unsnoc = (first MergeL <$>) . unsnoc . unMergeL
instance SeqCore (MergeCellsRight a) where
    unsnoc = (first MergeR <$>) . unsnoc . unMergeR

deriving via ASeqCore (MergeCellsLeft a) instance Foldable (MergeCellsLeft a)
deriving via ASeqCore (MergeCellsLeft a) instance ListLike (MergeCellsLeft a)
instance SeqLike (MergeCellsLeft a) where

instance Semigroup (MergeCellsLeft a) where (<>) = append
instance Semigroup (MergeCellsRight a) where (<>) = append
-}




instance ListCore (AsChars (Cells Unselected a)) where
    nil = AsChars nil
    singleton = AsChars . singleton . UnSel . T . singleton
    append = (<>)
    uncons (AsChars xs) = second AsChars <$> go xs
        where
        go :: Cells Unselected a -> Maybe (Char, Cells Unselected a)
        go Nil = Nothing
        go (UnSels (UnSel (c :< "") :< rest)) = Just (c, UnSels rest)
        go (UnSels (UnSel (c :< t) :< rest)) = Just (c, UnSels (UnSel t :< rest))
        -- TODO and with anything else, I'll need to retain non-char stuff at the front
instance ListCore (AsChars (Cells Selected a)) where
    nil = AsChars nil
    singleton c = AsChars $ InlSel (T $ singleton c) (Select (Left FloatCol))
    append = (<>)
    uncons (AsChars xs) = second AsChars <$> go xs
        where
        go :: Cells Selected a -> Maybe (Char, Cells Selected a)
        go Nil = Nothing
        go (InlSel (c :< "") _) = Just (c, Nil)
        go (InlSel (c :< t) bounds) = Just (c, InlSel t bounds)
        -- TODO and with anything else, I'll need to retain non-char stuff at the front
instance SeqCore (AsChars (Cells Unselected a)) where
    unsnoc (AsChars xs) = first AsChars <$> go xs
        where
        go :: Cells Unselected a -> Maybe (Cells Unselected a, Char)
        go Nil = Nothing
        go (UnSels (rest :> UnSel ("" :> c))) = Just (UnSels rest, c)
        go (UnSels (rest :> UnSel (t :> c))) = Just (UnSels (rest :> UnSel t), c)
        -- TODO and with anything else, I'll need to retain non-char stuff at the end
instance SeqCore (AsChars (Cells Selected a)) where
    unsnoc (AsChars xs) = first AsChars <$> go xs
        where
        go :: Cells Selected a -> Maybe (Cells Selected a, Char)
        go Nil = Nothing
        go (InlSel ("" :> c) _) = Just (Nil, c)
        go (InlSel (t :> c) bounds) = Just (InlSel t bounds, c)
        -- TODO and with anything else, I'll need to retain non-char stuff at the end

deriving via ASeqCore (AsChars (Cells Unselected a)) instance Foldable (AsChars (Cells Unselected a))
deriving via ASeqCore (AsChars (Cells Selected a)) instance Foldable (AsChars (Cells Selected a))

instance ListLike (AsChars (Cells Unselected a)) where
    length (AsChars cells) = getSum $ foldMap (Sum . cellCharLength_) cells
    splitAt n0 (AsChars cells) = both AsChars $ go n0 Nil cells
        where
        go 0 !acc rest = (acc, rest)
        go n !acc Nil = (acc, Nil)
        go n !acc (x :< rest)
            | n < l = case x of
                UnSel t -> let (x, y) = splitAt n t in (acc :> UnSel x, UnSel y :< rest)
            | otherwise = go (n - l) (acc :> x) rest
            where l = cellCharLength_ x
    -- FIXME what if take/drop is used on its own?
    -- I think this AsChars will disappear for AsCharbreaks, at which point nothing else can appear in here (until Marks...)
    -- in any case, only drop should have to be re-done
    -- TODO span
    -- TODO does anything else need looking after?
instance ListLike (AsChars (Cells Selected a)) where
    length (AsChars cells) = getSum $ foldMap (Sum . cellCharLength_) cells
    splitAt n0 (AsChars cells) = both AsChars $ go n0 Nil cells
        where
        go 0 !acc rest = (acc, rest)
        go n !acc Nil = (acc, Nil)
        go n !acc (x :< rest)
            | n < l = case x of
                InlSel1 t bounds -> let (x, y) = splitAt n t in (acc :> InlSel1 x bounds, InlSel1 y bounds :< rest)
            | otherwise = go (n - l) (acc :> x) rest
            where l = cellCharLength_ x
    -- FIXME drop
    -- TODO span
    -- TODO does anything else need looking after?
instance SeqLike (AsChars (Cells Unselected a)) where
    splitAtEnd n0 (AsChars cells) = both AsChars $ go n0 cells Nil
        where
        go 0 rest !acc = (rest, acc)
        go n Nil !acc = (Nil, acc)
        go n (rest :> x) !acc
            | n < l = case x of
                UnSel t -> let (x, y) = splitAtEnd n t in (rest :> UnSel x, UnSel y :< acc)
            | otherwise = go (n - l) rest (x :< acc)
            where l = cellCharLength_ x
    -- FIXME dropEnd needs the same work as for drop
    -- TODO span
    -- TODO does anything else need looking after?
instance SeqLike (AsChars (Cells Selected a)) where
    splitAtEnd n0 (AsChars cells) = both AsChars $ go n0 cells Nil
        where
        go 0 rest !acc = (rest, acc)
        go n Nil !acc = (Nil, acc)
        go n (rest :> x) !acc
            | n < l = case x of
                InlSel1 t bounds -> let (x, y) = splitAtEnd n t in (rest :> InlSel1 x bounds, InlSel1 y bounds :< acc)
            | otherwise = go (n - l) rest (x :< acc)
            where l = cellCharLength_ x
    -- FIXME dropEnd needs the same work as for drop
    -- TODO span
    -- TODO does anything else need looking after?



{-
instance ListLike (AsChars (Cells a)) where
    splitAt n0 (AsChars (Cells xs0)) = let (xs', ys') = go n0 xs0 in (AsChars (Cells xs'), AsChars (Cells ys'))
        where
        go :: Integral n => n -> Seq (Cell a) -> (Seq (Cell a), Seq (Cell a))
        go 0 ys = (Nil, ys)
        go n Nil = (Nil, Nil)
        go n (x :< xs)
            | l < n = let (xs', ys') = go (n - l) xs in (x :< xs', ys')
            | l == n = (singleton x, xs)
            | l > n = case x of
                UnSel t -> let (xs, ys) = splitAt n t in (singleton $ UnSel xs, singleton $ UnSel ys)
                InlSel t bounds -> let (xs, ys) = splitAt n t in (singleton $ InlSel xs bounds, singleton $ InlSel ys bounds)
            where
            l = cellCharLength_ x
    -- TODO span
instance SeqLike (AsChars (Cells a)) where
    splitAtEnd n0 (AsChars (Cells xs0)) = let (xs', ys') = go n0 xs0 in (AsChars (Cells xs'), AsChars (Cells ys'))
        where
        go :: Integral n => n -> Seq (Cell a) -> (Seq (Cell a), Seq (Cell a))
        go 0 xs = (xs, Nil)
        go n Nil = (Nil, Nil)
        go n (ys :> y)
            | l < n = let (xs', ys') = go (n - l) ys in (xs', ys' :> y)
            | l == n = (ys, singleton y)
            | l > n = case y of
                UnSel t -> let (xs, ys) = splitAtEnd n t in (singleton $ UnSel xs, singleton $ UnSel ys)
                InlSel t bounds -> let (xs, ys) = splitAtEnd n t in (singleton $ InlSel xs bounds, singleton $ InlSel ys bounds)
            where
            l = cellCharLength_ y
    -- TODO spanEnd
-}



cellCharLength_ :: Integral n => Cell sel a -> n
cellCharLength_ (UnSel t) = length t
cellCharLength_ (InlSel1 t _) = length t
-- TODO