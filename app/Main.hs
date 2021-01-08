-- todo can remove??
{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

{-# LANGUAGE RecursiveDo #-}
    -- allows recursive do notation
    -- mdo
    --     ...

{-# LANGUAGE TupleSections #-}

module Main 
    ( main
    ) where


import Linear 
import qualified SDL
import qualified SDL.Font
import qualified Data.Text as Text
import Data.List
import Data.Maybe

--import Data.Vector hiding (map, mapM, zip, take, sequence, splitAt, concat) -- Todo qualify

import System.Random
import Data.Word

import Foreign.C.Types (CInt)
import qualified Reactive.Banana as R
import qualified Reactive.Banana.Frameworks as R

-- Project imports
import SDL.Adapter
import Model
import Render



randomGems :: RandomGen g => Int -> g -> (g, [BoardCell])
randomGems 0 random = (random, [])
randomGems count random = (random'', (intToGem cell):cells)
  where (cell, random'') = randomR (1,5) random'
        (random', cells) = randomGems (count-1) random

        intToGem :: Int -> BoardCell
        intToGem 1 = GemCell Ruby
        intToGem 2 = GemCell Sapphire
        intToGem 3 = GemCell Emerald
        intToGem 4 = GemCell Pearl
        intToGem 5 = GemCell Amber
        intToGem _ = EmptyCell


getHighlight :: V2 Int -> Maybe CellCoords
getHighlight v = 
  let offsetV = V2 offset offset
      cellCoords' = (fromIntegral <$> (v ^-^ offsetV)) ^/ (fromIntegral gemSize) :: V2 Float
      cellCoords = ceiling <$> cellCoords'
  in 
    if inBounds cellCoords then
      Just cellCoords
    else
      Nothing

-- TODO remove??
toC (V2 x y) = (x, y)
toV (x, y) = V2 x y

applyBoardChange :: Board b => b -> BoardChange -> b
applyBoardChange board (SwapGems (V2 x1 y1) (V2 x2 y2)) = 
                changeCell leftCell board' (x1, y1)  
  where board' = changeCell rightCell board (x2, y2) 
        leftCell = cell board (x2, y2)
        rightCell = cell board (x1, y1)
applyBoardChange board (VanishGems gems) = foldl' (changeCell EmptyCell) board (map toC gems)
applyBoardChange board (SettleGems settlements newCells) = topOff newCells $ changeCells board $ reverse settlements -- this has to be reversed, otherwise extra cells will turn empty
  where changeCells orig ((coords, toFall):settlements) = changeCell EmptyCell board' (toC coords)
          where V2 column _ = coords
                board' = changeCell gem board (toC newLoc)
                newLoc = coords + (V2 0 toFall)
                gem = cell orig (toC coords)
                board = changeCells orig settlements
        changeCells orig [] = orig

        topOff newCells board = topOff' newCells board 1
        topOff' newCells board col
          | inBounds (V2 col 1) = topOff' newCells board' (col+1)
          | otherwise = board
          where board' = 
                  if (cell board topCoord == EmptyCell) then
                    changeCell newCell board topCoord
                  else board
                topCoord = (col, 1)
                newCell = newCells !! (col-1)




data Match = Match BoardCell Int

findLineMatches :: [((Int, Int), BoardCell)] -> [([(Int, Int)], Match)]
findLineMatches cells = foldl' match [] cells
  where match :: [([(Int, Int)], Match)] -> ((Int, Int), BoardCell) -> [([(Int, Int)], Match)]
        match [] (coord, cell) = [([coord], Match cell 1)]
        match ((coords, Match cell count):matches) (coord, cell')
          | cell == cell' = (coord:coords, Match cell (count + 1)):matches
          | otherwise = ([coord], Match cell' 1):(coords, Match cell count):matches
        
findBoardMatches :: Board b => b -> ([CellCoords], [Match])
findBoardMatches board = (map toV $ concat coords, matches)
  where (coords, matches) = unzip $ filter isScored indexedMatches

        isScored (_, Match (GemCell _) x) = x >= 3 --filter out matches that aren't 3 or more
        isScored (_, Match EmptyCell _) = False

        indexedMatches = concat [lineMatches $ rowItems board, lineMatches $ colItems board]
        lineMatches lines = concat $ map findLineMatches $ lines 
        toV (x, y) = V2 x y

-- Utility function pairs, [1, 2, 3, 4] -> [(1, 2), (2, 3), (3, 4)]
pairs [] = []
pairs xs = zip xs (tail xs)


findGemsToSettle' :: Int -> [((Int, Int), BoardCell)] -> [(CellCoords, Int)]
findGemsToSettle' emptyBelow ((_, EmptyCell):colRemain) = findGemsToSettle' (emptyBelow+1) colRemain
findGemsToSettle' 0 ((coords, cell):colRemain) = findGemsToSettle' 0 colRemain
findGemsToSettle' emptyBelow ((coords, cell):colRemain) = (toV coords, 1):findGemsToSettle' emptyBelow colRemain
findGemsToSettle' _ [] = []


findGemsToSettle :: Board b => b -> [(CellCoords, Int)]
findGemsToSettle board = concat $ map coords $ rowItems board -- TODO this should be colItems...
  where coords column = findGemsToSettle' 0 $ reverse column


main :: IO ()
main = do
  putStrLn "Init\n"

  SDL.initializeAll
  SDL.Font.initialize

  window <- SDL.createWindow "My SDL Application" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  font <- SDL.Font.load "Cascadia.ttf" 20

  eventSource <- getSDLEventSource

  let 
    initialRandom = mkStdGen 0
    initialInputState = InputState 
      { mouseCoords = V2 0 0
      , highlighted = Nothing
      , selected = Nothing
      }
    initialState = GameState 
      { board = emptyBoard (10, 10)
      , mode = Evaluating
      }

    networkDescription :: SDLEventSource -> R.MomentIO ()
    networkDescription eventSource = mdo
      current <- sdlEvent eventSource
      tick <- tickEvent eventSource

      let
        -- Behaviors derived from game state
        gameMode = mode <$> gameState
        boardB = board <$> gameState
        randomGemsB = snd <$> randomSource

        inputState :: R.Behavior (Maybe InputState)
        inputState = maybeInputState <$> gameMode
          where maybeInputState (Inputting inputState) = Just inputState
                maybeInputState _ = Nothing

        isInputting = isState <$> gameMode
          where isState (Inputting _) = True
                isState _ = False

        isChanging = isState <$> gameMode
          where isState (Changing _ _) = True
                isState _ = False

        isApplying = isState <$> gameMode
          where isState (Applying _) = True
                isState _ = False

        isEvaluating = isState <$> gameMode
          where isState (Evaluating) = True
                isState _ = False


        -- Mouse
        mouseButtonSDL :: R.Event SDL.MouseButtonEventData
        mouseButtonSDL = R.filterJust $ extract <$> current
          where extract (SDL.MouseButtonEvent e) = Just e
                extract _ = Nothing

        mouseLeftDown = R.filterE isLeftDown mouseButtonSDL
          where isLeftDown (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ _) = True
                isLeftDown _ = False

        mouseLeftUp = R.filterE isLeftUp mouseButtonSDL
          where isLeftUp (SDL.MouseButtonEventData _ SDL.Released _ SDL.ButtonLeft _ _) = True
                isLeftUp _ = False

        mouseMoveSDL :: R.Event SDL.MouseMotionEventData
        mouseMoveSDL = R.filterJust $ extract <$> current
          where extract (SDL.MouseMotionEvent e) = Just e
                extract _ = Nothing

        mouseMove :: Integral a => R.Event (V2 a)
        mouseMove = R.filterJust $ mouseCoords <$> mouseMoveSDL
          where mouseCoords (SDL.MouseMotionEventData _ _ _ (SDL.P point) _) = Just (fromIntegral <$> point)


        -- Logic

        -- When a cell is newly highlighted
        highlight = R.filterApply (isNewHighlight <$> gameMode) 
                      $ getHighlight <$> R.whenE isInputting mouseMove
          where isNewHighlight (Inputting (InputState _ highlight _)) highlight' = highlight /= highlight'

        -- When a highlighted cell is clicked
        selectedE = R.filterJust $ highlighted <$> (R.filterJust $ inputState R.<@ mouseLeftDown)

        -- A swap can only occur when a block is selected and another is highlighted
        getSwap :: GameMode -> Maybe BoardChange
        getSwap (Inputting (InputState _ h s)) = if h /= s then SwapGems <$> h <*> s else Nothing

        -- Perform a swap
        swap =  R.filterJust $ getSwap <$> gameMode R.<@ R.whenE isInputting mouseLeftUp

        -- Game State Updates TODO use lenses?
        updateHighlight coords inputState = inputState { highlighted = coords } 
        applyMouseUpdate point inputState = inputState { mouseCoords = point }
        applySelected selected inputState = inputState { selected = Just selected }

        applyInputState :: (InputState -> InputState) -> GameState -> GameState
        applyInputState f gameState = gameState { mode = Inputting inputState' }
          where inputState' = case mode gameState of
                                  Inputting inputState -> f inputState

        applyChangingMode change gameState = gameState { mode = Changing change 0.0 }

        applyGameMode :: GameMode -> GameState -> GameState
        applyGameMode gameMode gameState = gameState { mode = gameMode }

        -- The tick event for incrementing the change state
        -- TODO refactor: every state change triggers an event, every state change triggered by event
        updateChangingMode = update <$> gameMode R.<@ R.whenE isChanging tick
          where update (Changing change animProgress)
                  | animProgress >= 1.0 = Applying change
                  | otherwise = Changing change $ animProgress + 0.1

        -- Triggers when updateChangingMode triggers with the Applying mode
        newBoardStateE = (applyBoardChange <$> boardB) R.<@> (getChange <$> (gameMode R.<@ R.whenE isApplying tick))
          where getChange (Applying change) = change

        applyBoardState board gameState = gameState { 
                          board = board, 
                          mode = Evaluating } -- todo split up??

        evalTick = R.whenE isEvaluating tick

        -- Emit this event when 
        vanishGemsE = VanishGems <$> (R.filterE (not . null) $ fst <$> findBoardMatches <$> (boardB R.<@ evalTick))

        hasEmptyCells board = any ((==EmptyCell) . snd) (items board)

        gemsToSettleE = findGemsToSettle <$> (R.filterE hasEmptyCells $ boardB R.<@ evalTick)
        settleGemsE = ((flip SettleGems) <$> randomGemsB) R.<@> gemsToSettleE

        resetInput _ gameState = gameState { mode = Inputting initialInputState }
        resetInputE = R.filterE (not . hasEmptyCells) $ boardB R.<@ evalTick

      gameState <- R.accumB initialState $ 
                     R.unions [ 
                        applyBoardState <$> newBoardStateE
                      , applyChangingMode <$> settleGemsE
                      , applyChangingMode <$> vanishGemsE
                      , resetInput <$> resetInputE
                      , applyChangingMode <$> swap
                      , applyInputState <$> (R.whenE isInputting $ R.unions [
                          applySelected <$> selectedE
                        , applyMouseUpdate <$> mouseMove
                        , updateHighlight <$> highlight
                        ])
                      , applyGameMode <$> updateChangingMode
                      ]

      -- Get a new list of random gems and a new generator
      let nextRandom _ (random, gems) = randomGems 10 random
      randomSource <- R.accumB (randomGems 10 initialRandom) $ nextRandom <$> tick

      let
        renderEvent = render renderer font <$> gameState R.<@ tick

      R.reactimate renderEvent

  network <- R.compile $ networkDescription eventSource
  R.actuate network

  sdlEventPump 60 eventSource

  -- move ?
  SDL.Font.free font
  SDL.destroyRenderer renderer
  SDL.destroyWindow window