module Main where

import Board
import Board.Print

main :: IO ()
main = putStrLn . show $ initialBoardState
