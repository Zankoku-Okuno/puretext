module PureText.TextBuffer.Zipper.Base where


data Direction = Forwards | Backwards
    deriving(Eq, Read, Show)

data Landmark
    -- I like how the top and bottom ones are most complex and so take up a lot of the line, but going towards the middle gets simpler, taking up less of the line
    = SelectionTopLineStart
    | SelectionStart
    | LineStart
    | Here
    | LineEnd
    | SelectionEnd
    | SelectionBotLineEnd


-- when moving zipper carriage, how to interact with selection boundaries?
data Collision = Transparent | Solid

-- when moving elements across an in-selection zipper carriage, how to selection boundaries interact?
data Drag = Dominate | Submit | Hit




class Zippy z where
    type Base z :: * -- ^ type the zipper Ips across
    type Blocked z :: * -> * -- ^ construct an error type when the zipper can no longer move as requested
    type Elem z :: * -- ^ element that can be pushed and popped

    toZipper :: Direction -> Base z -> z
    fromZipper :: z -> Base z

    moveCarriage :: Direction -> z -> (Blocked z) z

    push :: Direction -> Elem z -> z -> z


data GoingHyperLine = GoHyper -- because, like jumping to hyperspace covers lots of space really fast, jumping to hyperline is manipulating a lot of lines really fast
