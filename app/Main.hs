module Main where

import Data.Foldable
import Data.List hiding (insert, delete)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

import Control.Exception
import Graphics.Vty
import qualified System.Console.Terminal.Size as TermSize
import PureText.Util

import PureText.TextBuffer.Lines
import PureText.TextBuffer.Zipper.Base
import qualified PureText.TextBuffer.Zipper.Slice as B
import qualified PureText.TextBuffer.Zipper.Slice.Buffer as B
import qualified PureText.Zipper.Core.LineSlices as Z
import qualified PureText.Zipper.Core.Text as Z
import PureText.TextBuffer.EditBuffer
-- import PureText.TextBuffer hiding (empty)
-- import qualified PureText.TextBuffer as E
-- import PureText.TextBuffer.Lines_Old (CharBreak(..))
-- import qualified PureText.TextBuffer.Lines_Old as L


main :: IO ()
main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg{mouseMode = Just True}
    Just w@TermSize.Window{..} <- TermSize.size
    let st0 = setTextFrame (showt w) $ resizeFrame (width, height) $ emptyFrame
    eventLoop vty st0 process draw `finally` shutdown vty
    print ("Goodbyte, cruel world!")



process :: Event -> Frame -> IO (Maybe Frame)
process (EvKey (KChar c) []) = plain $ \fr ->
    fr{frameLines = push Backwards (C c) (frameLines fr) }
process (EvKey KEnter []) = plain $ \fr ->
    fr{frameLines = push Backwards (Lb mempty) (frameLines fr) }
-- process (EvKey KBS []) = plain $ \fr ->
--     fr{frameLines = delete Backwards (frameLines fr) }
-- process (EvKey KDel []) = plain $ \fr ->
--     fr{frameLines = delete Forwards (frameLines fr) }
process (EvKey KLeft []) = plain $ \fr ->
    maybe fr (\frameLines -> fr{frameLines}) (moveCarriage Backwards (frameLines fr))
process (EvKey KRight []) = plain $ \fr ->
    maybe fr (\frameLines -> fr{frameLines}) (moveCarriage Forwards (frameLines fr))
process (EvKey KEsc []) = quit
process (EvResize w h) = plain $ resizeFrame (w, h)
process (EvMouseDown _ _ BScrollUp []) = plain $ scrollFrame (-1, 0)
process (EvMouseDown _ _ BScrollDown []) = plain $ scrollFrame (1, 0)
process (EvMouseDown _ _ BScrollUp [MShift]) = plain $ scrollFrame (0, -1)
process (EvMouseDown _ _ BScrollDown [MShift]) = plain $ scrollFrame (0, 1)
-- process ev = \x -> print ev >> pure (Just x)
process ev = pure . Just

plain :: (Frame -> Frame) -> Frame -> IO (Maybe Frame)
plain f = pure . Just . f
-- alterText :: (Text -> Text) -> Frame -> Frame
-- alterText f fr = flip setTextFrame fr . f . frameText $ fr
quit = const $ pure Nothing

draw :: Frame -> Picture
draw = picForImage . frameToImage


eventLoop :: Vty -> st -> (Event -> st -> IO (Maybe st)) -> (st -> Picture) -> IO ()
eventLoop vty st0 process draw = update vty (draw st0) >> loop st0
    where
    loop st = do
        ev <- nextEvent vty
        st' <- process ev st
        case st' of
            Just st' -> do
                let pic' = draw st'
                update vty pic'
                loop st'
            Nothing -> pure ()


showt :: Show a => a -> Text
showt = T.pack . show


----------------------------


data Frame = Frame
    { size :: (Int, Int) -- width, height
    , scroll :: (Int, Int) -- lines down, columns over
    , frameLines :: EditZipper ()
    }

emptyFrame :: Frame
emptyFrame = Frame
    { size = (0, 0)
    , scroll = (0, 0)
    , frameLines = toZipper Forwards mempty
    }

resizeFrame :: (Int, Int) -> Frame -> Frame
resizeFrame size' fr = fr{size = size'} -- FIXME change scroll to maintain percentage

scrollFrame :: (Int, Int) -> Frame -> Frame
scrollFrame (dDown, dOver) fr@Frame{..} = fr{scroll = (down, over)}
    where
    downLimits = (-10, 200) -- TODO (1 - h, Seq.length frameLines - 1)
    overLimits = (0, 500) -- TODO 0, maxLen + w - 1
    -- maxLen = maximum $ T.length <$> frameLines
    down = clamp downLimits $ down0 + dDown
    over = clamp overLimits $ over0 + dOver
    (down0, over0) = scroll
    (w, h) = size



-- snocFrame :: CharBreak -> Frame -> Frame
-- snocFrame c fr@Frame{frameLines = buf} = fr{frameLines = insert Backwards c buf }

-- unsnocFrame :: Frame -> Frame
-- unsnocFrame fr@Frame{frameLines = Empty} = fr
-- unsnocFrame fr@Frame{frameLines = rest :|> "" } = fr{frameLines = rest}
-- unsnocFrame fr@Frame{frameLines = rest :|> last } = fr{frameLines = rest :|> T.dropEnd 1 last}


setTextFrame :: Text -> Frame -> Frame
setTextFrame str fr = fr{frameLines = toZipper Forwards $ B.fromLines $ fromText "\n" str}

-- frameText :: Frame -> Text
-- frameText = T.unlines . toList . frameLines


-- FIXME oof, going straight to Text is linear performance, I should get logarithmic with Lines
frameToImage :: Frame -> Image
frameToImage Frame{..} = doCrop . doTranslate $ wholeImg
    where
    doTranslate = translate (negate over) (negate down)
    doCrop = crop w h
    doPad = pad (toPrepad over) (toPrepad down) w h
    wholeImg :: Image
    wholeImg = sillyToImg frameLines
    (w, h) = size
    (down, over) = scroll
    toPrepad i = if i < 0 then negate i else 0


sillyToImg :: EditZipper () -> Image
sillyToImg LineZ{..} = vertCat [fromWhole over, fromLSlices here, fromWhole under]
    where
    fromWhole (B.BS Empty) = emptyImage
    fromWhole (B.BS (B.Whole _ lines :<| Empty)) = text' defAttr $ toText "\n" lines
    fromLSlices (Z.LZ{here = Z.TZ t ""}) = horizCat
        [ text' curLineAttr t
        , text' curColAttr " "
        ]
    fromLSlices (Z.LZ{here = Z.TZ t (c :< t')}) = horizCat
        [ text' curLineAttr t
        , text' curColAttr (c :< "")
        , text' curLineAttr t'
        ]
    curLineAttr = defAttr `withStyle` bold
    curColAttr = curLineAttr `withStyle` underline