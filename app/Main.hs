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

import qualified SDL
import qualified SDL.Font

import qualified Data.Matrix as M
import qualified Data.Text as Text
import Data.List

import Linear 

--import Data.Vector hiding (map, mapM, zip, take, sequence, splitAt, concat) -- Todo qualify

import System.Random
import Data.Word
import Control.Monad (when, liftM)
import Foreign.C.Types (CInt)
import qualified Reactive.Banana as R
import qualified Reactive.Banana.Frameworks as R


-- Types
type EventSource a = (R.AddHandler a, a -> IO ())

addHandler :: EventSource a -> R.AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

data SDLEventSource = SDLEventSource { getSDLEvent :: EventSource SDL.EventPayload
                                     , getTickEvent :: EventSource Word32 }


-- | SDL event
sdlEvent :: SDLEventSource -> R.MomentIO (R.Event SDL.EventPayload)
sdlEvent = R.fromAddHandler . addHandler . getSDLEvent

-- | SDL tick
tickEvent :: SDLEventSource -> R.MomentIO (R.Event Word32)
tickEvent = R.fromAddHandler .  addHandler . getTickEvent



-- Core SDL loop
getSDLEventSource :: IO SDLEventSource
getSDLEventSource = SDLEventSource <$> R.newAddHandler <*> R.newAddHandler

fireSDLEvents :: SDLEventSource -> IO Bool
fireSDLEvents source = do
  let esdl = getSDLEvent source
      etick = getTickEvent source

      collectEvents :: IO (Maybe [SDL.EventPayload])
      collectEvents = do
        e <- SDL.pollEvent

        case fmap SDL.eventPayload e of 
          Just SDL.QuitEvent -> return Nothing
          Nothing -> return $ Just []
          Just event -> liftM (liftM (event:)) collectEvents

  tick <- SDL.ticks
  mEvents <- collectEvents
  case mEvents of Nothing -> return False
                  Just events -> do mapM (fire esdl) events
                                    fire etick tick
                                    return True

-- TODO how to multithread??
sdlEventPump :: Word16 -> SDLEventSource -> IO ()
sdlEventPump fps source = do
  startTick <- SDL.ticks
  shouldContinue <- fireSDLEvents source
  endTick <- SDL.ticks

  let ticks = fromIntegral $ endTick - startTick
      secondsPerFrame = fromIntegral $ 1000 `div` fps

  when (ticks < secondsPerFrame) $ SDL.delay(secondsPerFrame - ticks)
  when shouldContinue $ sdlEventPump fps source


renderRect :: Integral a => SDL.Point V2 a -> SDL.Renderer -> IO ()
renderRect point renderer = do
  SDL.fillRect renderer $ Just rect 
    where rect = SDL.Rectangle point' size
          point' = fromIntegral <$> point
          size = V2 10 10

renderHighlight :: Maybe (V2 Int) -> SDL.Renderer -> IO ()
renderHighlight (Just cellCoord) renderer = do
  let 
    point = SDL.P $ gemBasePos cellCoord
    size = V2 gemSize gemSize
    rect = SDL.Rectangle point size
    rect' = fromIntegral <$> rect

  SDL.fillRect renderer $ Just rect'
renderHighlight Nothing _ = pure ()

renderCell :: SDL.Renderer -> GameMode -> ((Int, Int), BoardCell) -> IO ()
renderCell renderer gameMode ((x, y), EmptyCell) = pure ()
renderCell renderer gameMode ((x, y), GemCell gem) = do
  let 
    cellCoord = V2 x y
    alpha' change animProgress = case change of
                      VanishGems coords ->
                        if any (==cellCoord) coords then -- TODO THIS IS SLOW!
                          floor (255 * (1 - animProgress))
                        else 255
                      _ -> 255
    alpha = case gameMode of
                Changing change animProgress -> alpha' change animProgress
                Applying change -> alpha' change 1.0
                _ -> 255

    color = case gem of
                Ruby -> V4 255 0 0 alpha
                Sapphire -> V4 0 0 255 alpha
                Emerald -> V4 0 255 0 alpha
                Pearl -> V4 255 255 255 alpha
                Amber -> V4 255 255 0 alpha

    point = SDL.P $ gemPos gameMode cellCoord
    size = (subtract 10) <$> V2 gemSize gemSize
    rect = SDL.Rectangle point size
    rect' = fromIntegral <$> rect

  SDL.rendererDrawColor renderer SDL.$= color
  SDL.fillRect renderer $ Just rect'

gemBasePos :: CellCoords -> V2 Int -- these don't need to be the same type?
gemBasePos cellCoord = ((cellCoord - V2 1 1) ^* gemSize) + (V2 offset offset)

gemAnimPos :: BoardChange -> Float -> CellCoords -> V2 Int
gemAnimPos (SwapGems left right) animProgress cellCoord
  | left == cellCoord = round <$> (lerp animProgress rightPos leftPos)
  | right == cellCoord = round <$> (lerp animProgress leftPos rightPos)
  | otherwise = gemBasePos cellCoord
  where leftPos = fromIntegral <$> (gemBasePos left) :: V2 Float
        rightPos = fromIntegral <$> (gemBasePos right) :: V2 Float
gemAnimPos (SettleGems coords) animProgress cellCoord
  -- TODO VVV THIS IS SLOW VVV 
  | any (==coordBelow) coords = round <$> (lerp animProgress bottomPos topPos )
  | otherwise = gemBasePos cellCoord
  where coordBelow = cellCoord ^+^ V2 0 1
        topPos = fromIntegral <$> gemBasePos cellCoord
        bottomPos = fromIntegral <$> gemBasePos coordBelow
gemAnimPos _ _ cellCoord = gemBasePos cellCoord

gemPos :: GameMode -> CellCoords -> V2 Int
gemPos (Changing change animProgress) cellCoord = gemAnimPos change animProgress cellCoord
gemPos (Applying change) cellCoord = gemAnimPos change 1.0 cellCoord
gemPos _ cellCoord = gemBasePos cellCoord


drawDebugInfo :: SDL.Renderer -> SDL.Font.Font -> GameMode -> IO ()
drawDebugInfo renderer font gameMode = do
  let
    modeText = case gameMode of
      Inputting _ -> "Inputting"
      Changing (SwapGems _ _) animProgress -> "Changing SwapGems" ++ show animProgress
      Changing (VanishGems _) animProgress -> "Changing VanishGems" ++ show animProgress
      Changing (SettleGems _) animProgress -> "Changing SettleGems" ++ show animProgress
      Applying (SwapGems _ _) -> "Applying SwapGems"
      Applying (VanishGems _) -> "Applying VanishGems"
      Applying (SettleGems _) -> "Applying SettleGems"
      Evaluating -> "Evaluating"

    textColor = V4 128 255 128 255
    textOrigin = SDL.P $ V2 0 0

    -- Changing _ ticksLeft -> do
    --   let ticksText = Text.pack $ show ticksLeft  

  sdlText <- SDL.Font.blendedWrapped font textColor 150 $ Text.pack modeText
  textTex <- SDL.createTextureFromSurface renderer sdlText 
  textureInfo <- SDL.queryTexture textTex

  let width = SDL.textureWidth textureInfo
      height = SDL.textureHeight textureInfo

  SDL.copy renderer textTex Nothing $ Just $ fromIntegral <$> (SDL.Rectangle textOrigin $ V2 width height)
  

-- Rendering
render :: SDL.Renderer -> SDL.Font.Font -> GameState -> IO ()
render renderer font gameState = do
  SDL.rendererDrawColor renderer SDL.$= V4 80 80 80 255
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
  SDL.clear renderer

  let 
    gameMode = mode gameState
    drawInputState = case gameMode of -- TODO use Maybe/IO transformer?
      Inputting (InputState mouseCoords selected highlight) -> do
        SDL.rendererDrawColor renderer SDL.$= V4 128 128 128 255
        renderHighlight highlight renderer

        SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255
        renderHighlight selected renderer
      _ -> pure ()

  drawInputState

  sequence $ map (renderCell renderer gameMode) $ items $ board gameState

  drawDebugInfo renderer font gameMode

  -- SDL.rendererDrawColor renderer SDL.$= V4 255 0 255 255
  -- renderRect (mouseCoords gameMode) renderer

  SDL.present renderer

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


newtype MatrixBoard = MatrixBoard (M.Matrix BoardCell)

instance Board MatrixBoard where
  cell (MatrixBoard m) coord = m M.! coord
  items (MatrixBoard m) = M.toList $ M.mapPos (,) m
  rowItems (MatrixBoard m) = M.toLists $ M.mapPos (,) m
  colItems (MatrixBoard m) = M.toLists $ M.transpose $ M.mapPos (,) m
  changeCell cell (MatrixBoard m) coords = MatrixBoard $ M.setElem cell coords m

enumerate x = zip [0..] x

-- newtype VectorBoard = VectorBoard (Vector (Vector BoardCell))

-- instance Board VectorBoard where
--   cell (VectorBoard v) (x, y) = (v ! y) ! x

--   items (VectorBoard v) = [ ((x, y), c) 
--                | (y, row) <- enumerate (toList v)
--                , (x, c) <- enumerate (toList row) ]

--   changeCell (VectorBoard v) (x, y) cell = VectorBoard $ v // [(y, row)]
--     where row = (v ! y) // [(x, cell)]


genBoard :: RandomGen g => g -> MatrixBoard
genBoard random = MatrixBoard $ intToGem <$> m

  where m = M.fromList gridSize gridSize $ randomRs (1,5) random

        intToGem :: Int -> BoardCell
        intToGem 1 = GemCell Ruby
        intToGem 2 = GemCell Sapphire
        intToGem 3 = GemCell Emerald
        intToGem 4 = GemCell Pearl
        intToGem 5 = GemCell Amber
        intToGem _ = EmptyCell



data BoardChange = 
    SwapGems (CellCoords) (CellCoords)   -- coords of two gems to swap
  | VanishGems [CellCoords]              -- list of gems to vanish
  | SettleGems [CellCoords] -- extents of gems to settle??

data InputState = InputState { mouseCoords :: SDL.Point V2 Int
                             , highlighted :: Maybe (V2 Int)
                             , selected :: Maybe (V2 Int)
                             }


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

type CellCoords = V2 Int


getHighlight :: SDL.Point V2 Int -> Maybe CellCoords
getHighlight (SDL.P v) = 
  let offsetV = V2 offset offset
      cellCoords' = (fromIntegral <$> (v ^-^ offsetV)) ^/ (fromIntegral gemSize) :: V2 Float
      cellCoords = ceiling <$> cellCoords'
  in 
    if inBounds cellCoords then
      Just cellCoords
    else
      Nothing

toC (V2 x y) = (x, y)

applyBoardChange :: Board b => b -> BoardChange -> b
applyBoardChange board (SwapGems (V2 x1 y1) (V2 x2 y2)) = 
                changeCell leftCell board' (x1, y1)  
  where board' = changeCell rightCell board (x2, y2) 
        leftCell = cell board (x2, y2)
        rightCell = cell board (x1, y1)
applyBoardChange board (VanishGems gems) = foldl' (changeCell EmptyCell) board (map toC gems)
applyBoardChange board (SettleGems coords) = foldl' changeCells board coords
  where changeCells board coord = changeCell EmptyCell board' $ coordsAbove 
          where board' = changeCell cellAbove board $ toC coord
                cellAbove = cell board coordsAbove 
                coordsAbove = toC $ coord ^-^ (V2 0 1)


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

findGemsToSettle :: Board b => b -> [CellCoords]
findGemsToSettle board = concat $ map coords $ rowItems board -- TODO this should be colItems...
  where coords column = foldl' accum [] $ pairs column
        accum coords (((_, GemCell _)), ((x, y), EmptyCell)) = (V2 x y):coords
        accum coords _ = coords


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
    random = mkStdGen 0
    initialInputState = InputState 
      { mouseCoords = SDL.P $ V2 0 0
      , highlighted = Nothing
      , selected = Nothing
      }
    initialState = GameState 
      { board = genBoard random
      , mode = Inputting initialInputState
      }

    networkDescription :: SDLEventSource -> R.MomentIO ()
    networkDescription eventSource = mdo
      current <- sdlEvent eventSource
      tick <- tickEvent eventSource

      let
        -- Behaviors derived from game state
        gameMode = mode <$> gameState
        boardB = board <$> gameState

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

        mouseMove :: Integral a => R.Event (SDL.Point V2 a)
        mouseMove = R.filterJust $ mouseCoords <$> mouseMoveSDL
          where mouseCoords (SDL.MouseMotionEventData _ _ _ point _) = Just (fromIntegral <$> point)


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

        --settleGemsE = SettleGems <$> (R.filterE (not . null) $ findGemsToSettle <$> (boardB R.<@ evalTick))


      gameState <- R.accumB initialState $ 
                     R.unions [ 
                        --applyChangingMode <$> settleGemsE
                        applyBoardState <$> newBoardStateE
                      , applyChangingMode <$> vanishGemsE
                      , applyChangingMode <$> swap
                      , applyInputState <$> (R.whenE isInputting $ R.unions [
                          applySelected <$> selectedE
                        , applyMouseUpdate <$> mouseMove
                        , updateHighlight <$> highlight
                        ])
                      , applyGameMode <$> updateChangingMode
                      ]

      mousePosition <- R.stepper (SDL.P $ V2 0 0) mouseMove

      let
        renderEvent = render renderer font <$> gameState R.<@ tick

      R.reactimate renderEvent

  network <- R.compile $ networkDescription eventSource
  R.actuate network

  sdlEventPump 10 eventSource

  -- move ?
  SDL.Font.free font
  SDL.destroyRenderer renderer
  SDL.destroyWindow window