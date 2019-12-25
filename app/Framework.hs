module Framework where

import PureText.Prelude

import System.IO
import Control.Exception
import Graphics.Vty
import qualified System.Console.Terminal.Size as TermSize


type Startup st = IO st
type Update st = Event -> st -> IO (Maybe st)
type Draw st = st -> Picture
type App st = (Startup st, Update st, Draw st)

mainApp :: App st -> IO ()
mainApp app = do
    cfg <- standardIOConfig
    vty <- mkVty cfg{mouseMode = Just True}
    -- Just w@TermSize.Window{..} <- TermSize.size
    -- let st0 = setTextFrame (showt w) $ resizeFrame (width, height) $ emptyFrame
    eventLoop vty app `finally` shutdown vty

eventLoop :: Vty -> App st -> IO ()
eventLoop vty (mkSt0, process, draw) = do
    st0 <- mkSt0
    update vty (draw st0)
    loop st0
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


startupSize :: Integral a => (TermSize.Window a -> st) -> Startup st
startupSize f = do
    Just size <- TermSize.size
    pure $ f size

testingUpdate :: (Event -> st -> st) -> Update st
testingUpdate handler = filterQuit isEsc $ \ev st -> pure $ handler ev st

filterQuit :: (Event -> Bool) -> (Event -> st -> IO st) -> Update st
filterQuit p upd ev st
    | p ev = pure Nothing
    | otherwise = Just <$> upd ev st

isEsc :: Event -> Bool
isEsc (EvKey (KEsc) _) = True
isEsc _ = False
