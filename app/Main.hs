module Main where

import System.Random
import qualified Data.Vector.Generic as G
import Board
import Board.Print
import Moves

main :: IO ()
main = playRandomGame White initialBoardState

showInitialBoard :: IO ()
showInitialBoard = putStrLn . show $ initialBoardState

playRandomGame :: Color -> BoardState -> IO ()
playRandomGame c bs =
    do result <- playRandomMove c bs
       case result of
         Just newBs -> playRandomGame (opponent c) newBs
         Nothing    -> pure ()

playRandomMove :: Color -> BoardState -> IO (Maybe BoardState)
playRandomMove c bs =
    do putStrLn "Play move"
       _ <- getLine
       let moves = getAllMoves c bs
       if G.length moves == 0
          then pure Nothing
          else do 
              i <- randomRIO (0, (G.length moves) - 1)
              let move = moves G.! i
              let newBs = applyMove bs move
              putStrLn . show $ newBs
              pure $ Just newBs 
