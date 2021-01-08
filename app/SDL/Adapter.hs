module SDL.Adapter(
    sdlEvent, 
    tickEvent,
    getSDLEventSource,
    sdlEventPump,
    SDLEventSource
  ) where


import qualified Reactive.Banana as R
import qualified Reactive.Banana.Frameworks as R
import qualified SDL

import Control.Monad (when, liftM)
import Data.Word


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