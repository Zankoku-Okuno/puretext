module Main.WidthTest where

import PureText.Prelude
import Framework

import qualified Data.Text as T
import qualified Data.Sequence as Seq

import Graphics.Vty


app :: App St
app = (pure st0, filterQuit isQuit updDebug, draw)


data St = St
    { partA :: Text
    , partB :: Maybe Text
    , evLine :: Text
    }

st0 :: St
st0 = (St "Hello, world! mi水zu" Nothing "")

updDebug ev st = do
    st' <- upd ev st
    pure st'{evLine = T.pack . show $ ev}

upd ev@(EvMouseUp col 0 (Just BLeft)) st@St{partB = Nothing} = pure $
    let (a', b') = splitAt col $ partA st
    in st{partA = a', partB = Just b'}
upd (EvKey KEnter _) st@(St{partA = a, partB = Just b}) = pure $ st{partA = a <> b, partB = Nothing}
upd ev st = pure st

draw (St a b ev) = picForImage $ vertCat $ text' defAttr <$> [a, fromMaybe "" b, ev]


isQuit (EvKey (KEsc) _) = True
isQuit _ = False





-- process :: Event -> Frame -> IO (Maybe Frame)
-- process (EvKey (KChar c) []) = plain $ \fr ->
--     fr{frameLines = push Backwards (C c) (frameLines fr) }
-- process (EvKey KEnter []) = plain $ \fr ->
--     fr{frameLines = push Backwards (Lb mempty) (frameLines fr) }
-- -- process (EvKey KBS []) = plain $ \fr ->
-- --     fr{frameLines = delete Backwards (frameLines fr) }
-- -- process (EvKey KDel []) = plain $ \fr ->
-- --     fr{frameLines = delete Forwards (frameLines fr) }
-- process (EvKey KLeft []) = plain $ \fr ->
--     maybe fr (\frameLines -> fr{frameLines}) (moveCarriage Backwards (frameLines fr))
-- process (EvKey KRight []) = plain $ \fr ->
--     maybe fr (\frameLines -> fr{frameLines}) (moveCarriage Forwards (frameLines fr))
-- process (EvKey KEsc []) = quit
-- process (EvResize w h) = plain $ resizeFrame (w, h)
-- process (EvMouseDown _ _ BScrollUp []) = plain $ scrollFrame (-1, 0)
-- process (EvMouseDown _ _ BScrollDown []) = plain $ scrollFrame (1, 0)
-- process (EvMouseDown _ _ BScrollUp [MShift]) = plain $ scrollFrame (0, -1)
-- process (EvMouseDown _ _ BScrollDown [MShift]) = plain $ scrollFrame (0, 1)
-- -- process ev = \x -> print ev >> pure (Just x)
-- process ev = plain id

-- plain :: (Frame -> Frame) -> Frame -> IO (Maybe Frame)
-- plain f = pure . Just . f
-- quit = const $ pure Nothing

-- draw :: Frame -> Picture
-- draw = picForImage . frameToImage




-- showt :: Show a => a -> Text
-- showt = T.pack . show


-- ----------------------------


-- data Frame = Frame
--     { size :: (Int, Int) -- width, height
--     , scroll :: (Int, Int) -- lines down, columns over
--     , frameLines :: EditZipper ()
--     }

-- emptyFrame :: Frame
-- emptyFrame = Frame
--     { size = (0, 0)
--     , scroll = (0, 0)
--     , frameLines = toZipper Forwards mempty
--     }

-- resizeFrame :: (Int, Int) -> Frame -> Frame
-- resizeFrame size' fr = fr{size = size'} -- FIXME change scroll to maintain percentage

-- scrollFrame :: (Int, Int) -> Frame -> Frame
-- scrollFrame (dDown, dOver) fr@Frame{..} = fr{scroll = (down, over)}
--     where
--     downLimits = (-10, 200) -- TODO (1 - h, Seq.length frameLines - 1)
--     overLimits = (0, 500) -- TODO 0, maxLen + w - 1
--     -- maxLen = maximum $ T.length <$> frameLines
--     down = clamp downLimits $ down0 + dDown
--     over = clamp overLimits $ over0 + dOver
--     (down0, over0) = scroll
--     (w, h) = size

-- setTextFrame :: Text -> Frame -> Frame
-- setTextFrame str fr = fr{frameLines = toZipper Forwards $ B.fromLines $ fromText "\n" str}


-- -- FIXME oof, going straight to Text is linear performance, I should get logarithmic with Lines
-- frameToImage :: Frame -> Image
-- frameToImage Frame{..} = doCrop . doTranslate $ wholeImg
--     where
--     doTranslate = translate (negate over) (negate down)
--     doCrop = crop w h
--     doPad = pad (toPrepad over) (toPrepad down) w h
--     wholeImg :: Image
--     wholeImg = sillyToImg frameLines
--     (w, h) = size
--     (down, over) = scroll
--     toPrepad i = if i < 0 then negate i else 0


-- sillyToImg :: EditZipper () -> Image
-- sillyToImg Z.LineZ{..} = vertCat [fromWhole over, fromLSlices here, fromWhole under]
--     where
--     fromWhole (B.BS Empty) = emptyImage
--     fromWhole (B.BS (B.Whole _ lines :<| Empty)) = text' defAttr $ toText "\n" lines
--     fromLSlices (Z.LZ{here = Z.TZ t ""}) = horizCat
--         [ text' curLineAttr t
--         , text' curColAttr " "
--         ]
--     fromLSlices (Z.LZ{here = Z.TZ t (c :< t')}) = horizCat
--         [ text' curLineAttr t
--         , text' curColAttr (c :< "")
--         , text' curLineAttr t'
--         ]
--     curLineAttr = defAttr `withStyle` bold
--     curColAttr = curLineAttr `withStyle` underline
