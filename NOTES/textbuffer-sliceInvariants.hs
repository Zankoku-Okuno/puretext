

{-
Annoyingly, I think it's important to have O(1) access to the start/end of the buffer.
That is, I'd prefer to be able to loop over selection top-to-botton (and vice-versa)
    while maintaining the currently-focused selection.
E.e. look at text of all selected lines, then decide whether to modify those texts uniformly.
-}




data Shape = Inline | Enter | Exit

data LineSlice (f :: Shape) (b :: Shape) where
    T :: Text -> LineSlice 'Inline 'Inline
    InlSel :: Text -> Bounds -> LineSlice 'Inline 'Inline
    StartSel :: Text -> LineSlice 'Inline 'Enter
    EndSel :: Text -> LineSlice 'Exit 'Inline
    -- Mark :: LineSlice a a
    -- MoreStartSel :: Text -> LineSlice 'Enter 'Enter
    -- MoreEndSel :: Text -> LineSlice 'Exit 'Exit


data BufferSlice a (f :: Shape) (b :: Shape) where
    Cutup :: SliceList LineSlice f b -> LineHydration a -> BufferSlice f b
    Whole :: Lines a -> BufferSlices 'Inline 'Inline
    MlSel :: Lines a -> Bounds -> BufferSlice a 'Enter 'Exit



data SliceList (t :: Shape -> Shape -> *) (f :: Shape) (b :: Shape) where
    Nil :: SliceList t f b
    Cons :: t f x -> SliceList t x b -> SliceList t f b

data SliceTsil (t :: Shape -> Shape -> *) (f :: Shape) (b :: Shape) where
    Lin :: SliceTsil t f b
    Snoc :: SliceTsil t f x -> t x b -> SliceTsil t f b




type BufferSlices a f b = SliceList (BufferSlice a) f b
type ReffubSlices a f b = SliceList (BufferSlice a) f b

type LineSlices a f b = SliceList (LineSlice a) f b
type EnilSlices a f b = SliceList (LineSlice a) f b


data Buffer a
    = Inline
        { above :: ReffubSlices a 'Inline 'Inline
        , leftEdge :: EnilSlices 'Inline 'Inline
        , here :: (Text, Bounds)
        , rightEdge :: LineSlices 'Inline 'Inline
        , below :: BufferSlices a 'Inline 'Inline
        }
    | Multiline
        { above :: ReffubSlices a 'Inline x
        , early :: EnilSlices x 'Enter
        , near :: BufferSlice a 'Enter 'Exit
        , late :: SliceList (LineSlice a) 'Exit y
        , below :: ReffubSlices a y 'Inline
        }
