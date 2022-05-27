-- This module defines the UI logic of our Minesweeper program, based on the
-- game logic in src/Minesweeper.hs and the Brick library for terminal UI.

-- This UI is very minimal, but note that it only allows the user to input
-- valid indices on the board, which is very convenient for our Grid type.

module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Table
import Control.Applicative
import Control.Monad
import Data.Finite (natToFinite)
import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.Text qualified as Text
import Data.Void
import GHC.TypeNats
import Graphics.Vty.Attributes (Attr, defAttr)
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..), Button(..))

import Grid
import Minesweeper

-- This is the core type of values that will *change* throughout the execution
-- of the UI thread. The focus field stores the currently-selected cell in the
-- UI, and gets updated when the user selects a different cell.
data AppState w h where
  AppState ::
    { cover :: Cover w h
    , focus :: Index w h
    } -> AppState w h


-- A Brick UI widget to represent a single cell on the game board. Surrounds
-- the cell text with a border if it's selected, or one space of padding if
-- it's unselected.
cellWidget ::
  Bool -> Index w h ->
  FieldCell -> SurveyCell -> CoverCell ->
  Widget Void
cellWidget selected i fc sc cc =
  let
    cellText = prettyGameCell fc sc cc
    baseWidget =
      withAttr (attrName (Text.unpack cellText)) $
        txt cellText
  in
    if selected then
      border baseWidget
    else
      padAll 1 baseWidget

-- A Brick UI widget to represent an entire game board as a table of cells.
-- Puts borders between each cell.
boardWidget ::
  KnownBounds w h =>
  Field w h -> AppState w h -> Widget Void
boardWidget field st =
  let survey = surveyField field in
    renderTable $ table $ gridToLists $
      cellWidget
        <$> fmap (\i -> i == focus st) idGrid
        <*> idGrid
        <*> field
        <*> surveyField field
        <*> cover st

-- When the user presses an arrow key, these up/down/left/right functions
-- compute the next focus location. The Finite type does modular arithmetic
-- with the + and - operators, so we get wrap-around behavior in the UI from
-- these definitions; if we didn't want that behavior, we could use the inc and
-- dec functions from the Grid module to define versions of each of these that
-- return a Maybe (Index w h).

up :: (KnownBounds w h, 2 <= h) => Index w h -> Index w h
up (c,r) = (c, r - natToFinite @1 Proxy)

down :: (KnownBounds w h, 2 <= h) => Index w h -> Index w h
down (c,r) = (c, r + natToFinite @1 Proxy)

left :: (KnownBounds w h, 2 <= w) => Index w h -> Index w h
left (c,r) = (c - natToFinite @1 Proxy, r)

right :: (KnownBounds w h, 2 <= w) => Index w h -> Index w h
right (c,r) = (c + natToFinite @1 Proxy, r)

-- The "<=" in these constraints actually means "is less than or equal to",
-- which you might not expect in this context since it looks like a reversed
-- version of "=>". These constraints are required in order to write
-- natToFinite @1 Proxy, because 1 is not a valid value of Finite 0 or
-- Finite 1, only Finite 2 and above. In practice, these constraints line up
-- nicely with the needs of our UI: if either dimension is 1 or 0, then it
-- *should* be impossible to move the cursor in that dimension.


-- Update the UI after the user selects a cell by pressing the Enter key while
-- the cell is highlighted. If the cell is already uncovered, there's nothing
-- to update.
selectCell ::
  KnownBounds w h =>
  Field w h -> Survey w h ->
  AppState w h -> EventM Void (Next (AppState w h))
selectCell field survey st =
  continue $ case index (cover st) (focus st) of
    Covered -> st { cover = uncoverCell (focus st) survey (cover st) }
    _ -> st
     

flagCell ::
  KnownBounds w h =>
  Field w h -> Survey w h ->
  AppState w h -> EventM Void (Next (AppState w h))
flagCell field survey st =
  continue $ case index (cover st) (focus st) of
    Uncovered -> st
    _ -> st { cover = replace (focus st) Flagged (cover st) }


-- Handle a BrickEvent, which represents a user input or some other change in
-- the terminal state outside our application. Brick gives us two relevant
-- commands in the EventM monad:
--   halt takes a final AppState and exits the UI thread
--   continue takes a next AppState and continues the UI thread
handleEvent ::
  (KnownBounds w h, 2 <= w, 2 <= h) =>
  Field w h -> Survey w h ->
  AppState w h -> BrickEvent Void Void ->
  EventM Void (Next (AppState w h))
handleEvent field survey st event =
  case event of
    -- The VtyEvent constructor with an EvKey argument indicates that the user
    -- has pressed a key on the keyboard. The empty list in the pattern
    -- indicates that no modifier keys (Shift/Ctrl/...) were being held down
    -- while the key was pressed.
    VtyEvent (EvKey key []) ->
      case key of
        KLeft -> continue $ st { focus = left (focus st) }
        KRight -> continue $ st { focus = right (focus st) }
        KUp -> continue $ st { focus = up (focus st) }
        KDown -> continue $ st { focus = down (focus st) }
        KEsc -> halt st
        KEnter -> selectCell field survey st
        KChar ' ' -> flagCell field survey st
        _ -> continue st
    -- We don't care about any other kind of events, at least in this version
    -- of the code.
    _ -> continue st


-- The attribute map for our application, which Brick uses to apply text styles
-- to our widgets. If you want to change the color scheme, the color values are
-- defined here:
--   hackage.haskell.org/package/vty/docs/Graphics-Vty-Attributes-Color.html
gameAttrMap :: AttrMap
gameAttrMap =
  attrMap
    (brightWhite `on` black) -- default scheme for names not listed below
    [ (attrName "1", fg brightBlue)
    , (attrName "2", fg green)
    , (attrName "3", fg brightRed)
    , (attrName "4", fg blue)
    , (attrName "5", fg red)
    , (attrName "6", fg cyan)
    , (attrName "7", fg brightBlack)
    , (attrName "8", fg white)
    ]

-- The Brick application definition, which is used to generate our main
-- function. appChooseCursor and appStartEvent are given "default" values that
-- just opt out of those Brick features. appDraw calls boardWidget to render
-- the UI, appHandleEvent calls handleEvent to respond to user inputs, and
-- gameAttrMap defines the text style of the UI.
app ::
  (KnownBounds w h, 2 <= w, 2 <= h) =>
  Field w h -> Survey w h ->
  App (AppState w h) Void Void
app field survey =
  App
    { appDraw = \st -> [boardWidget field st]
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent = handleEvent field survey
    , appStartEvent = pure
    , appAttrMap = \_ -> gameAttrMap
    }


-- The application state when the game starts: all cells are covered, and the
-- focus is in the upper-left. Requires at least one cell in the grid, because
-- the focus has to be somewhere.
initialAppState :: (KnownBounds w h, 1 <= w, 1 <= h) => AppState w h
initialAppState =
  AppState
    { cover = pure Covered
    , focus = (natToFinite @0 Proxy, natToFinite @0 Proxy)
    }


-- Finally, to run the game, we just generate a random field and let Brick do
-- its work. When the UI thread exits, we check the final board state to see if
-- the user lost or won, and print out the final board state.
main :: IO ()
main = do
  field <- randomField @8 @10 7
  finalAppState <- defaultMain (app field (surveyField field)) initialAppState
  when (gameLost field (cover finalAppState)) $
    putStrLn "you lose!"
  when (gameWon field (cover finalAppState)) $
    putStrLn "you win!"
  putStrLn "final board:"
  printGameBoard field (cover finalAppState)
