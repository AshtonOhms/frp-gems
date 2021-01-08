module Render (
    render
    ) where


import qualified Data.Text as Text
import qualified SDL
import qualified SDL.Font

import Data.List
import Data.Maybe

import Linear

import Model

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
                          floor (255 * (max 0 $ 1 - animProgress))
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
gemAnimPos (SettleGems settlements _) animProgress cellCoord
  -- TODO VVV THIS IS SLOW VVV 
  | isJust matchingSettlement = round <$> (lerp animProgress bottomPos topPos )
  | otherwise = gemBasePos cellCoord
  where coordBelow = cellCoord ^+^ (V2 0 $ fromMaybe 0 toFall)
        topPos = fromIntegral <$> gemBasePos cellCoord
        bottomPos = fromIntegral <$> gemBasePos coordBelow
        toFall = snd <$> matchingSettlement
        matchingSettlement = find (\(coord, _) -> coord == cellCoord) settlements
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
      Changing (SwapGems _ _) animProgress -> "Changing\nSwapGems\n" ++ show animProgress
      Changing (VanishGems _) animProgress -> "Changing\nVanishGems\n" ++ show animProgress
      Changing (SettleGems _ _) animProgress -> "Changing\nSettleGems\n" ++ show animProgress
      Applying (SwapGems _ _) -> "Applying\nSwapGems"
      Applying (VanishGems _) -> "Applying\nVanishGems"
      Applying (SettleGems _ _) -> "Applying\nSettleGems"
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
    gameMode = _mode gameState
    drawInputState = case gameMode of -- TODO use Maybe/IO transformer?
      Inputting (InputState mouseCoords selected highlight) -> do
        SDL.rendererDrawColor renderer SDL.$= V4 128 128 128 255
        renderHighlight highlight renderer

        SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255
        renderHighlight selected renderer
      _ -> pure ()

  drawInputState

  sequence $ map (renderCell renderer gameMode) $ items $ _board gameState

  drawDebugInfo renderer font gameMode

  -- SDL.rendererDrawColor renderer SDL.$= V4 255 0 255 255
  -- renderRect (mouseCoords gameMode) renderer

  SDL.present renderer