module Model(
    gemSize,
    offset,
    gridSize,
    inBounds,
    Gem (..),
    BoardChange(..),
    BoardCell (..),
    GameState (..),
    GameMode (..),
    InputState (..),
    CellCoords,
    Board (..),
    MatrixBoard
  ) where

import qualified Data.Matrix as M

import Linear 


----------
-- Game Structures
----------

-- Board
gemSize :: Int
gemSize = 50

offset :: Int
offset = 50

gridSize :: Int
gridSize = 10

inBounds :: CellCoords -> Bool
inBounds (V2 x y) = (b x) && (b y) where b a = (a >= 1) && (a <=(gridSize))

type CellCoords = V2 Int

data Gem = Ruby | Sapphire | Emerald | Pearl | Amber
  deriving (Eq, Ord)

data BoardCell = EmptyCell | GemCell Gem
  deriving (Eq, Ord)


class Board a where
  cell :: a -> (Int, Int) -> BoardCell
  items :: a -> [((Int, Int), BoardCell)]
  rowItems :: a -> [[((Int, Int), BoardCell)]]
  colItems :: a -> [[((Int, Int), BoardCell)]]
  changeCell :: BoardCell -> a -> (Int, Int) -> a
  emptyBoard :: (Int, Int) -> a 


newtype MatrixBoard = MatrixBoard (M.Matrix BoardCell)

instance Board MatrixBoard where
  cell (MatrixBoard m) coord = m M.! coord
  items (MatrixBoard m) = M.toList $ M.mapPos (,) m
  rowItems (MatrixBoard m) = M.toLists $ M.mapPos (,) m
  colItems (MatrixBoard m) = M.toLists $ M.transpose $ M.mapPos (,) m
  changeCell cell (MatrixBoard m) coords = MatrixBoard $ M.setElem cell coords m
  emptyBoard (width, height) = MatrixBoard m
    where m = M.fromList width height $ repeat EmptyCell


-- Game State
data GameState = GameState 
  { board :: MatrixBoard
  , mode :: GameMode
  }

-- TODO rename GamePhase?
data GameMode = 
  Inputting InputState
  | Changing BoardChange Float
  | Applying BoardChange
  | Evaluating

data InputState = InputState { mouseCoords :: V2 Int
                             , highlighted :: Maybe (V2 Int)
                             , selected :: Maybe (V2 Int)
                             }


data BoardChange = 
    SwapGems (CellCoords) (CellCoords)   -- coords of two gems to swap
  | VanishGems [CellCoords]              -- list of gems to vanish
  | SettleGems [(CellCoords, Int)] [BoardCell] -- gems to settle, by how much, new gems




