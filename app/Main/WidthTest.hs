module Main.WidthTest where

import PureText.Prelude
import Framework

import qualified Data.Text as T
import qualified Data.Sequence as Seq

import Graphics.Vty


app :: App St
app = (pure st0, testingUpdate updDebug, draw)


data St = St
    { partA :: Text
    , partB :: Text
    , evLine :: Maybe Event
    }

st0 :: St
st0 = (St "He水ll水o, 水wo水rl水d! 水i水zu水" "" Nothing)

updDebug ev st = (upd ev st){evLine = Just ev}

upd ev@(EvMouseUp col 0 (Just BLeft)) st@St{partB = ""} =
    -- let (a', b') = splitAt col $ partA st
    let (a', b') = wcSplitAt col $ partA st
    in st{partA = a', partB = b'}
upd (EvKey KEnter _) st@(St{partA, partB}) = st{partA = partA <> partB, partB = ""}
upd ev st = st

draw (St a b ev) = picForImage $ vertCat $ text' defAttr <$> [a, b, maybe "" (T.pack . show) ev]





{- So, tabs are handled very poorly by wc width.
    I'm thinking I'll need to put an additional layer of translation.
    Essentially, I'll catch all the weird chars when I render text.
    Then, when translating a point back to an index into text, I'll
        use wcwidth on the same replacements as they are detected,
        or fall back to wcwidth for unreplaced chars.
-}

wcToIndex :: Int -> Text -> Int
-- wcToIndex n t = length (wcTake n t)
wcToIndex n0 = go (0, n0)
    where
    go (!ix, !rem) "" = ix + rem
    go (!ix, !rem) (c :< t)
        | rem < wc = ix
        | otherwise = go (ix + 1, rem - wc) t
        where wc = safeWcwidth c


wcSplitAt :: Int -> Text -> (Text, Text)
wcSplitAt n t = splitAt (wcToIndex n t) t


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
