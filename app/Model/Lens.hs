{-# LANGUAGE TemplateHaskell #-}

module Model.Lens where

import Control.Lens

import Model

makeLenses ''InputState

makePrisms ''GameMode

makeLenses ''GameState