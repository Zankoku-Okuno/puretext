{-# LANGUAGE UndecidableInstances #-}
module PureText.Holding where

import Data.Function
import Data.Tuple

import Text.Show(Show(..))
import Data.String(IsString(..))


newtype Holding a = Hold { unHold :: a }


-- the naming with Hold&friends is just…too cute
class Hold a where
    type Held a
    type Hand a

    grip :: Hand a -> Held a -> a

    ungrip :: a -> (Hand a, Held a)
    theHand :: a -> Hand a
    theHand = fst . ungrip
    theHeld :: a -> Held a
    theHeld = snd . ungrip

    holdMap :: (Held a -> Held a) -> Holding a -> Holding a
    holdMap f (Hold x) = holdWith x $ f (theHeld x)

    holdWith :: Hold a => a -> Held a -> Holding a
    holdWith it = Hold . grip (theHand it)


class Hold a => HandsFree a where
    gripZero :: Held a -> a



instance (Show (Held a), HandsFree a) => Show (Holding a) where
    show = show . theHeld . unHold
instance (IsString (Held a), HandsFree a) => IsString (Holding a) where
    fromString = Hold . gripZero . fromString