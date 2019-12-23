module PureText.Select.Zipper where
{-
import PureText.Prelude

import PureText.Select.Base
import PureText.Select.Buffer
import PureText.Select.Line
import PureText.Select.LineCell
import PureText.Select.Text


data Zipper a
    = Inline
        { over :: Buffer a
        -- selection lines --
            , left :: LineCells
            , here :: Textish
            , bounds :: Bounds
            , right :: LineCells
            , h2o :: a
        -- end selection lines --
        , under :: Buffer a
        }
    | Multiline
        { over :: Buffer a
        -- selection lines --
            -- top line
            , left :: LineCells
            , top :: Textish
            , bounds :: Bounds
            , top_h2o :: a
            -- central lines
            , mid :: Seq (Line a)
            -- bottom line
            , bot :: Textish
            , right :: LineCells
            , bot_h2o :: a
        -- end selection lines --
        , under :: Buffer a
        }


-- insertLeft
-- deleteLeft
-- deleteRight

-- collapseSelection
-- dragSelectionLeft
-- dragSelectionRight

-- TODO manipulate primary selection

-- TODO vertical operations (moveUp, moveDown, dragUp, dragDown)
-- TODO large operations (by word, by line, by file, &c)
-- TODO queries
-}