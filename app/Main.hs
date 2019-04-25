module Main where

import           System.Random
import           Data.Maybe                     ( isNothing
                                                , fromJust
                                                )
import qualified Data.Vector.Generic           as G
import           Board
import           Moves
import           Search

main :: IO ()
main = playBestGame White initialBoardState 

playBestGame :: Color -> BoardState -> IO ()
playBestGame c bs = do
    putStrLn . show $ bs 
    result <- playBestMove c bs
    case result of
        Just newBs -> playBestGame (opponent c) newBs
        Nothing    -> pure ()

playAgainstRobot :: Color -> Color -> BoardState -> IO ()
playAgainstRobot humancolor c bs = do
    putStrLn . show $ bs
    result <- if humancolor == c then playHumanMove c bs else playBestMove c bs
    case result of
        Just newBs -> playAgainstRobot humancolor (opponent c) newBs
        Nothing    -> pure ()

playBestMove :: Color -> BoardState -> IO (Maybe BoardState)
playBestMove c bs = do
    putStrLn "Play best move:"
    _ <- getLine
    let result = getBestMove bs c
    if isNothing result
        then pure Nothing
        else do
            let move = fromJust result
            putStrLn "Best move:"
            putStrLn $ show move
            putStrLn ""
            let newBs = applyMove bs move
            pure $ Just newBs

playHumanMove :: Color -> BoardState -> IO (Maybe BoardState)
playHumanMove c bs = do
    putStrLn "Enter move:"
    move <- fmap read $ getLine
    let newBs = applyMove bs move
    pure $ Just newBs
