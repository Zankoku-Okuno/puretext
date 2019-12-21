module PureText.Select.Base where

import PureText.Prelude


-- TODO use later
-- data Dirt = Clean | Dirty

data Charbreak = C Char | Lb

-- TODO use later
-- data Mark
--     = StackMark Word Direction -- mark stored at depth on stack
--     | RegMark Char Direction -- named mark
-- data Mark = Marks (Map (Either Word Char) Bounds)) -- with the invariant that the Select bounds are always FloatCol?

-- type Marked a = Either Mark a


data Bounds
    = Cursor
    | Select (Either ColIx ColIx)
data ColIx
    = FloatCol
    | FixedCol Int
    | FloatEnd

newtype AsChars t = AsChars { unChars :: t }
    deriving(Semigroup, Monoid)
newtype AsCharish t = AsCharish { unCharish :: t}
    deriving(Semigroup, Monoid)
newtype AsCharbreaks t = AsCharbreaks { unCharbreaks :: t }
    deriving(Semigroup, Monoid)
newtype AsLines t = AsLines { unLines :: t }
    deriving(Semigroup, Monoid)
