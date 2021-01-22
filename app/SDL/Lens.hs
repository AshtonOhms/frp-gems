{-# LANGUAGE TemplateHaskell #-}

module SDL.Lens where

import Control.Lens
import SDL

makePrisms ''EventPayload