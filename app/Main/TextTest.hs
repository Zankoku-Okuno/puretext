module Main.TextTest where

import PureText.Prelude hiding ((<|>))
import PureText.Select.Base
import PureText.Select.Text
import PureText.Select.Cell

import qualified Data.Text as T
import Graphics.Vty hiding (Cursor)
import Framework
import System.Console.Terminal.Size (Window(..))



data Buffer = B
    { before :: Cells Unselected ()
    , here :: Cells Selected ()
    , after :: Cells Unselected ()
    }

data St = St
    { buf :: Buffer
    , debug :: Maybe Event
    , size :: (Int, Int)
    }
st0 Window{height, width} = St
    { buf = B nil nil nil
    , debug = Nothing
    , size = (height, width)
    }

app :: App St
app = (startupSize st0, upd, draw)


upd = testingUpdate updEv
updEv ev b = upd' ev b{debug = Nothing}
upd' ev@(EvKey (KChar '\t') []) st@St{buf} = st{debug = Just ev}
upd' (EvKey (KChar c) []) st@St{buf} = st{buf = insert (singleton c) buf}
upd' (EvKey KBS []) st@St{buf} = st{buf = delete Backwards buf}
upd' (EvKey KDel []) st@St{buf} = st{buf = delete Forwards buf}
upd' (EvKey KLeft []) st@St{buf} = st{buf = move Backwards buf}
upd' (EvKey KLeft [MShift]) st@St{buf} = st{buf = drag Backwards buf}
upd' (EvKey KRight []) st@St{buf} = st{buf = move Forwards buf}
upd' (EvKey KRight [MShift]) st@St{buf} = st{buf = drag Forwards buf}
upd' ev st = st{debug = Just ev}

draw st = picForImage $ drawBuf (buf st) <-> drawDebug (debug st)
    where
    drawBuf (B before here after) = pad 0 0 w 0 drawLine
        where
        drawLine = imgBefore <|> imgHere <|> imgAfter <|> final
            where
            (imgBefore, endCurBefore) = drawCells False before
            (imgHere, endCurHere) = drawCells endCurBefore here
            (imgAfter, endCurAfter) = drawCells endCurHere after
            final = text' (if endCurAfter then withCursorStyle defAttr else defAttr) " "
        drawCell :: Bool -> Cell sel a -> (Image, Bool)
        drawCell startCur (UnSel t@(c :< t')) = (img, False)
            where
            img = case startCur of
                True -> text' (withCursorStyle defAttr) (singleton c) <|> text' defAttr (unT t')
                False -> text' defAttr (unT t)
        drawCell startCur (InlSel1 "" bounds) = (emptyImage, endsWithCursor)
            where
            endsWithCursor = case bounds of { Select (Left _) -> False ; _ -> True }
        drawCell startCur (InlSel1 t@(c :< t') bounds) = (img, endsWithCursor)
            where
            img = case (startCur, bounds) of
                (True, _) ->
                    text' (withCursorStyle $ withSelectStyle defAttr) (singleton c) <|>
                    text' (withSelectStyle defAttr) (unT t')
                (False, Select (Left _)) ->
                    text' (withCursorStyle $ withSelectStyle defAttr) (singleton c) <|>
                    text' (withSelectStyle defAttr) (unT t')
                (False, _) -> text' (withSelectStyle defAttr) (unT t)
            endsWithCursor = case bounds of { Select (Left _) -> False ; _ -> True }
        drawCells :: Bool -> Cells sel a -> (Image, Bool)
        drawCells startCur (UnSels cells) = foldl go (emptyImage, startCur) cells
            where go (img, endCur) cell = first (img <|>) $ drawCell endCur cell
        drawCells startCur PointSel = (emptyImage, True)
        drawCells startCur (InlSel t@(c :< t') b) = (img, endCur)
            where
            img = case (startCur, b) of
                (True, _) ->
                    text' (withCursorStyle $ withSelectStyle defAttr) (singleton c) <|>
                    text' (withSelectStyle defAttr) (unT t')
                (False, Select (Left _)) ->
                    text' (withCursorStyle $ withSelectStyle defAttr) (singleton c) <|>
                    text' (withSelectStyle defAttr) (unT t')
                _ -> text' (withSelectStyle defAttr) (unT t)
            endCur = case b of { Select (Right _) -> True ; _ -> False }
    drawDebug Nothing = emptyImage
    drawDebug (Just ev) = text' defAttr $ (T.pack . show) ev
    (h, w) = size st

withSelectStyle attr = attr `withStyle` underline
withCursorStyle attr = attr `withStyle` standout





move :: Direction -> Buffer -> Buffer
move d buf@B{here = Nil} = case d of
    Forwards -> buf{before = before', after = after'}
        where
        (AsChars over, AsChars after') = splitAt 1 (AsChars $ after buf)
        before' = before buf <> over
    Backwards -> buf{before = before', after = after'}
        where
        (AsChars before', AsChars over) = splitAtEnd 1 (AsChars $ before buf)
        after' = over <> after buf
move d buf = case d of
    Forwards -> buf{before = before buf <> deselect (here buf), here = Nil}
    Backwards -> buf{here = Nil, after = deselect (here buf) <> after buf}

drag :: Direction -> Buffer -> Buffer
-- FIXME dragging forwards with left bounds, and the opposite
drag Forwards buf = case cellBounds (here buf) of
    Select (Left _) -> buf{before = before', here = here'}
        where
        (AsChars outof, AsChars here') = splitAt 1 (AsChars $ here buf)
        before' = before buf <> deselect outof
    _ -> buf{here = here', after = after'}
        where
        (AsChars into, AsChars after') = splitAt 1 (AsChars $ after buf)
        here' = alterBounds (const bounds') $ here buf <> select bounds' into
        bounds' = Select (Right FloatCol)
drag Backwards buf = case cellBounds (here buf) of
    Select (Right _) -> buf{here = here', after = after'}
        where
        (AsChars here', AsChars outof) = splitAtEnd 1 (AsChars $ here buf)
        after' = deselect outof <> after buf
    _ -> buf{before = before', here = here'}
        where
        (AsChars before', AsChars into) = splitAtEnd 1 (AsChars $ before buf)
        here' = alterBounds (const bounds') $ select bounds' into <> here buf
        bounds' = Select (Left FloatCol)


insert :: Text -> Buffer -> Buffer
insert t b = b{before = before b `snoc` UnSel (T t), here = nil}

delete :: Direction -> Buffer -> Buffer
delete d b@B{here = Nil} = case d of
    Forwards -> b{after = unChars $ drop 1 (AsChars $ after b)}
    Backwards -> b{before = unChars $ dropEnd 1 (AsChars $ before b)}
delete _ b = b{here = Nil}


data Direction = Forwards | Backwards
