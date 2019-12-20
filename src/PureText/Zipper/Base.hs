{- |
Common types and interfaces for the various zippers in "PureText.Zipper".
-}
module PureText.Zipper.Base where

{- |
The 'PureText.Zipper.Edit.EditZipper' is ac omplex zipper, relying on
    several more zippers for its implementation.
This class collects the primitive functions on such zippers
    so that I don't have to suffix every similar function with the type on which it operates.

WARNING: I am still coming to grips with the exact set of primitives needed.
-}
class Zippy z where
    {- | Type the zipper zips across. -}
    type Base z :: *
    {- | Construct a possible error type to account for
        the possibility that the zipper carriage can no longer move as requested.
    -}
    type Blocked z :: * -> *
    {- | The type of element that can be pushed and popped into the zipper. -}
    type Elem z :: *

    {- | Convert the 'Base' type into a zipper, which starts its carriage
        from the front on 'Forwards', and
        from the back on 'Backwards'.
    -}
    toZipper :: Direction -> Base z -> z
    {- | Reconstruct the 'Base' type from the zipper. 

        Obviously, this must be a total function, or else the conversion to zipper
            would have lost information.
    -}
    fromZipper :: z -> Base z

    {- | Move the carriage one position forwards or backwards.
        This may or may not correspond to moving across an 'Elem',
            since there may be points in the zipper that are not 'Elem's.
    -}
    moveCarriage :: Direction -> z -> (Blocked z) z

    {- | Place an 'Elem' into the zipper either 'Forwards' or 'Backwards' of the carriage.
    -}
    push :: Direction -> Elem z -> z -> z

    {- | Attempt to pop an 'Elem' off the zipper, but it might be 'Blocked'

        If 'moveCarriage' would not move over an 'Elem', it is suggested to return a 'mempty' 'Blocked'.
        If a client wishes to pop past some non-'Elem', they may move past the blockage to pop.
    -}
    pop :: Direction -> z -> (Blocked z) (Elem z, z)


{- |
Since we deal with many zippers that can push\/pop on two sides,
    and often have pairs of fingers, we use this type
    rather than rely on blind booleans.

WARNING: Nevertheless, this type has gotten used occaisionally to describe
    relative ordering of different parts of a zipper,
    which feels a bit like \"blind direction\".
-}
data Direction = Forwards | Backwards
    deriving(Eq, Read, Show)

{- |
Replaces an otherwise blind unit type when
'moveCarriage' on 'PureText.Zipper.HyperLine.HyperLineZipper' or 'PureText.Zipper.LineSlices.LineSlicesZipper'
reports that it is not entirely blocked, but must move to\/from hyperline.

The naming follows the hyperspace analogy laid out in (FIXME where am I going to describe this analogy?).
-}
data GoingHyperLine = GoHyper -- because, like jumping to hyperspace covers lots of space really fast, jumping to hyperline is manipulating a lot of lines really fast


-- data Landmark
--     -- I like how the top and bottom ones are most complex and so take up a lot of the line, but going towards the middle gets simpler, taking up less of the line
--     = SelectionTopLineStart
--     | SelectionStart
--     | LineStart
--     | Here
--     | LineEnd
--     | SelectionEnd
--     | SelectionBotLineEnd


-- -- when moving zipper carriage, how to interact with selection boundaries?
-- data Collision = Transparent | Solid

-- -- when moving elements across an in-selection zipper carriage, how to selection boundaries interact?
-- data Drag = Dominate | Submit | Hit


