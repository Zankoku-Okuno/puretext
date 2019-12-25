module Main.TextTest where

import PureText.Prelude hiding ((<|>))
import PureText.Select.Base
import PureText.Select.Text

import qualified Data.Text as T
import Graphics.Vty hiding (Cursor)
import Framework
import System.Console.Terminal.Size (Window(..))



data Buffer = B
    { before :: Textish
    , here :: Textish
    , bounds :: Bounds
    , after :: Textish
    }

data St = St
    { buf :: Buffer
    , debug :: Maybe Event
    , size :: (Int, Int)
    }
st0 Window{height, width} = St
    { buf = B nil nil Cursor nil
    , debug = Nothing
    , size = (height, width)
    }

app :: App St
app = (startupSize st0, upd, draw)


upd = testingUpdate updEv
updEv ev b = upd' ev b{debug = Nothing}
upd' ev@(EvKey (KChar '\t') []) st@St{buf} = st{debug = Just ev}
upd' (EvKey (KChar c) []) st@St{buf} = st{buf = insert c buf}
upd' (EvKey KBS []) st@St{buf} = st{buf = delete Backwards buf}
upd' (EvKey KDel []) st@St{buf} = st{buf = delete Forwards buf}
upd' (EvKey KLeft []) st@St{buf} = st{buf = move Backwards buf}
upd' (EvKey KLeft [MShift]) st@St{buf} = st{buf = drag Backwards buf}
upd' (EvKey KRight []) st@St{buf} = st{buf = move Forwards buf}
upd' (EvKey KRight [MShift]) st@St{buf} = st{buf = drag Forwards buf}
upd' ev st = st{debug = Just ev}

draw st = picForImage $ drawBuf (buf st) <-> drawDebug (debug st)
    where
    drawBuf (B before here bounds after) = pad 0 0 w 0 (b <|> h <|> a)
        where
        b = text' defAttr (unT before)
        h = case (bounds, here) of
            (Select (Left _), c :< t) ->
                text' (withCursorStyle $ withSelectStyle defAttr) (singleton c) <|>
                text' (withSelectStyle defAttr) (unT t)
            (_, t) -> text' (withSelectStyle defAttr) (unT t)
        a = case (bounds, uncons after) of
            (Select (Left _), _) -> text' defAttr (unT after)
            (_, Nothing) -> text' (withCursorStyle defAttr) " "
            (_, Just (c, t)) ->
                text' (withCursorStyle defAttr) (singleton c) <|>
                text' defAttr (unT t)
    drawDebug Nothing = emptyImage
    drawDebug (Just ev) = text' defAttr $ (T.pack . show) ev
    (h, w) = size st

withSelectStyle attr = attr `withStyle` underline
withCursorStyle attr = attr `withStyle` standout





move :: Direction -> Buffer -> Buffer
move d buf@B{here = Nil} = case d of
    Forwards -> buf{before = before', after = after'}
        where (before', after') = xferLeft 1 (before buf, after buf)
    Backwards -> buf{before = before', after = after'}
        where (before', after') = xferRight 1 (before buf, after buf)
move d buf = case d of
    Forwards -> buf{before = (before buf) <> (here buf), here = Nil, bounds = Cursor}
    Backwards -> buf{here = Nil, after = (here buf) <> (after buf), bounds = Cursor}

drag :: Direction -> Buffer -> Buffer
drag Forwards buf = case bounds buf of
    Select (Left _) -> buf{before = before', here = here', bounds = bounds'}
        where
        (before', here') = xferLeft 1 (before buf, here buf)
        bounds' = case here' of { Nil -> Cursor ; _ -> bounds buf }
    _ -> buf{here = here', after = after', bounds = bounds'}
        where
        (here', after') = xferLeft 1 (here buf, after buf)
        bounds' = Select (Right FloatCol)
drag Backwards buf = case bounds buf of
    Select (Right _) -> buf{here = here', after = after', bounds = bounds'}
        where
        (here', after') = xferRight 1 (here buf, after buf)
        bounds' = case here' of { Nil -> Cursor ; _ -> bounds buf }
    _ -> buf{before = before', here = here', bounds = bounds'}
        where
        (before', here') = xferRight 1 (before buf, here buf)
        bounds' = Select (Left FloatCol)

insert :: Char -> Buffer -> Buffer
insert c b@B{here = Nil} = b{before = before b `snoc` c, here = Nil}
insert c b = insert c $ delete Backwards b

delete :: Direction -> Buffer -> Buffer
delete d b@B{here = Nil} = case d of
    Forwards -> b{after = drop 1 (after b)}
    Backwards -> b{before = dropEnd 1 (before b)}
delete _ b = b{here = Nil, bounds = Cursor}



xferRight :: Int -> (Textish, Textish) -> (Textish, Textish)
xferRight n (before, after) =
    let (before', over) = splitAtEnd n before
        after' = over `append` after
    in (before', after')

xferLeft :: Int -> (Textish, Textish) -> (Textish, Textish)
xferLeft n (before, after) =
    let (over, after') = splitAt n after
        before' = before `append` over
    in (before', after')


data Direction = Forwards | Backwards