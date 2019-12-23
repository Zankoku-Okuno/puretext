{-# LANGUAGE UndecidableInstances #-}
module Util.DerivingVia.Newtypesque where

import Prelude
import Data.String (IsString(..))

newtype Newtypesque a = Newtype { unNewtype :: a }
class IsNewtypesque new a | new -> a where
    injNewtype :: a -> new
    prjNewtype :: new -> a


instance (IsString a, IsNewtypesque new a) => IsString (Newtypesque new) where
    fromString = Newtype . injNewtype . fromString

instance (Show a, IsNewtypesque new a) => Show (Newtypesque new) where
    show = show . prjNewtype . unNewtype
