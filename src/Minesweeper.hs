-- This module defines the game logic for our implementation of the game
-- Minesweeper, based on the Grid type in src/Grid.hs.

-- The provided code here is a very minimal Minesweeper implementation: there
-- is no support for the "flag" or "question mark" feature of common
-- Minesweeper implementations, or several other common features. That can be
-- your job!

module Minesweeper where

import Control.Applicative
import Control.Monad.State
import Data.Finite
import Data.Foldable (toList, for_)
import Data.List (uncons)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Traversable (for)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeNats
import System.Random (randomRIO)

import Grid

-- There are three core concepts in our Minesweeper algorithms, which I've
-- somewhat arbitrarily named "field", "cover", and "survey". (I'm not sure if
-- there are common terms for these things, they're hard to search for.)


-- A Field is a Grid that represents where each mine is on the game board.

data FieldCell where
  Mine :: FieldCell
  NoMine :: FieldCell
  deriving (Show, Eq)

type Field w h = Grid w h FieldCell


-- A Cover is a Grid that represents the visibility of each cell on the game
-- board (whether the cell's value is hidden to the user in the UI).

data CoverCell where
  Covered :: CoverCell
  Uncovered :: CoverCell
  Flagged :: CoverCell
  deriving (Show, Eq)

type Cover w h = Grid w h CoverCell


-- A Survey is a Grid that represents the number shown on each cell on the game
-- board when the cell is visible to the user.

type SurveyCell = Int

type Survey w h = Grid w h SurveyCell

-- We could use the type Finite 9 as our definition of SurveyCell, since each
-- cell has at most 8 adjacent cells, but we don't need any guarantees about
-- SurveyCell values in order to know that our program won't crash, and trying
-- to prove anything meaningful about our "surveying" algorithm would require
-- proof capabilities beyond what the Data.Finite module offers (at least to do
-- relatively cleanly).


-- Decide if a player has lost a game: have they uncovered any cells that are
-- mines?
gameLost ::
  KnownBounds w h =>
  Field w h -> Cover w h -> Bool
gameLost field cover =
  any (\(fc,cc) -> fc == Mine && cc == Covered) $
    liftA2 (,) field cover

-- Decide if the player has won a game: have they uncovered all the cells that
-- aren't mines?
gameWon ::
  KnownBounds w h =>
  Field w h -> Cover w h -> Bool
gameWon field cover =
  all (\(fc,cc) -> fc == Mine || cc == Uncovered) $
    liftA2 (,) field cover

-- "Survey" a single field cell: how many mines are in it, zero or one?
surveyCell :: FieldCell -> SurveyCell
surveyCell Mine = 1
surveyCell NoMine = 0

-- "Survey" a whole field: how many mines are adjacent to each cell? We compute
-- the survey results even for cells containing mines, even though the UI
-- doesn't use those results, just because it's convenient for the
-- implementation of this "surveying" algorithm.
surveyField ::
  forall w h.
  KnownBounds w h =>
  Field w h -> Survey w h
surveyField = fmap (sum . fmap surveyCell) . neighborhoods


-- Convert a cell on the game board to a string representing its state:
--   "#" represents any covered cell
--   "@" represents an uncovered mine
--   " " represents a cell with zero adjacent mines
--   "n" represents a cell with n adjacent mines (for 0 < n <= 8)
prettyGameCell :: FieldCell -> SurveyCell -> CoverCell -> Text
prettyGameCell _ _ Flagged = Text.pack "!"
prettyGameCell _ _ Covered = Text.pack "#"
prettyGameCell Mine _ Uncovered = Text.pack "@"
prettyGameCell NoMine 0 Uncovered = Text.pack " "
prettyGameCell NoMine n Uncovered = Text.pack $ show n

-- Convert a game board to a string representing its state. Use with
-- Text.putStr, or use printGameBoard.
prettyGameBoard ::
  forall w h.
  KnownBounds w h =>
  Field w h -> Cover w h -> Text
prettyGameBoard field cover =
  Text.unlines $ Vector.toList $
    fmap (Text.concat . Vector.toList) $
      gridCells $
        liftA3 prettyGameCell field (surveyField field) cover

-- Prints a game board to standard console output. This is not actually used in
-- the UI code, since Brick handles the rendering there; this is just handy for
-- debugging in the REPL.
printGameBoard ::
  forall w h.
  KnownBounds w h =>
  Field w h -> Cover w h -> IO ()
printGameBoard field cover = Text.putStr $ prettyGameBoard field cover 


-- The type of state in our "uncovering" search algorithm, which is a
-- depth-first graph search:
--   seen: the set of indices we've already visited
--   unseen: the working stack of indices
--   currentCover: the state of the cover, which we uncover cells in as we go
data SearchState w h where
  SearchState ::
    { seen :: Set (Index w h)
    , unseen :: [Index w h]
    , currentCover :: Cover w h
    } -> SearchState w h

-- Pop an index off of the top of the working stack.
pop ::
  forall w h.
  State (SearchState w h) (Maybe (Index w h))
pop = do
  stack <- fmap (uncons . unseen) get
  for stack $ \(top,rest) -> do
    modify $ \st -> st { unseen = rest }
    pure top



-- Returns true if the CoverCell type is flagged else false
isFlagged :: CoverCell -> Bool
isFlagged Flagged = True
isFlagged _ = False


replaceWhereTrue ::
  forall w h f a.
  KnownBounds w h =>
  Index w h -> CoverCell -> (CoverCell -> Bool) -> Grid w h CoverCell -> Grid w h CoverCell
replaceWhereTrue i x func =
  imap (\j y -> if (elem j (neighborIndices i) && func y) then x else y)



-- Search for all zero-mine-count cells connected to the index at the top of
-- the working stack, and uncover each of them and their neighborhoods.
uncoverState ::
  forall w h.
  KnownBounds w h =>
  Survey w h -> State (SearchState w h) ()
uncoverState survey = do
  top <- pop
  for_ top $ \i -> do
    s <- get
    when (not (isFlagged (index (currentCover s) i))) $ do
      modify $ \st ->
        st
          { currentCover = replace i Uncovered (currentCover st)
          , seen = Set.insert i (seen st)
          }
    when (index survey i == 0 && not (isFlagged (index (currentCover s) i))) $ do
      modify $ \st ->
        let
          unseenNeighbors =
            filter
              (\j -> Set.notMember j (seen st))
              (neighborIndices i)
        in
          st
            { currentCover = replaceWhereTrue i Uncovered (not . isFlagged) (currentCover st)
            , unseen = unseenNeighbors ++ unseen st
            }
    uncoverState survey


-- Run the action of a user uncovering a single cell in the game, which will
-- also uncover more cells via uncoverState if the selected cell is a
-- zero-mine-count cell. This doesn't check whether the selected cell contains
-- a mine in the field, that's handled in the UI.
uncoverCell ::
  forall w h.
  KnownBounds w h =>
  Index w h -> Survey w h -> Cover w h -> Cover w h
uncoverCell i survey cover =
  currentCover $
    execState (uncoverState survey) $
      SearchState
        { seen = Set.empty
        , unseen = [i]
        , currentCover = cover
        }


-- Choose a random element from a set, or return Nothing if the set is empty.
-- This is helpful in populating a random Field.
chooseRandom :: forall a. StateT (Set a) IO (Maybe a)
chooseRandom = do
  xs <- get
  if null xs then
    pure Nothing
  else do
    i <- lift (randomRIO (0, Set.size xs - 1))
    modify (Set.deleteAt i)
    pure (Just (Set.elemAt i xs))

-- Populate a Field with the given number of mines placed in random locations,
-- making sure that the randomly-chosen mine locations don't overlap.
populateField ::
  forall w h.
  KnownBounds w h =>
  Int -> StateT (Set (Index w h)) IO (Field w h)
populateField mineCount =
  if mineCount == 0 then
    pure (pure NoMine)
  else do
    field <- populateField (mineCount - 1)
    choice <- chooseRandom
    pure (case choice of
      Nothing -> field
      Just i -> replace i Mine field)

-- Generate a random Field from scratch by populating a field with all possible
-- indices as choices for random selection.
randomField ::
  forall w h.
  KnownBounds w h =>
  Int -> IO (Field w h)
randomField mineCount =
  evalStateT
    (populateField mineCount)
    (Set.fromList (toList idGrid))
