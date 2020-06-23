{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Control.Monad
import Data.Maybe

import Board 
import Solver

main = do
  json <- getJson "ten-boards.json"
  let boards = readBoards json
      solved = (map solve) <$> boards
      mb = join $ sequence <$> solved

  prr mb


