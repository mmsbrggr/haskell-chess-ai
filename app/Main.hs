module Main where

import           System.Environment
import           Data.Maybe                     ( isNothing
                                                , fromJust
                                                , maybe
                                                )
import           Board
import           Moves
import           Search

main :: IO ()
main = getNextMove

getNextMove :: IO ()
getNextMove = do
    fen : color : _ <- getArgs
    let bs     = getBoardStateFromFEN fen
    let c      = if color == "White" then White else Black
    let result = getBestMove bs c
    print $ maybe "ERROR" show result

playBestGame :: Color -> BoardState -> IO ()
playBestGame c bs = do
    print bs
    result <- playBestMove c bs
    case result of
        Just newBs -> playBestGame (opponent c) newBs
        Nothing    -> pure ()

playAgainstRobot :: Color -> Color -> BoardState -> IO ()
playAgainstRobot humancolor c bs = do
    print bs
    result <- if humancolor == c then playHumanMove c bs else playBestMove c bs
    case result of
        Just newBs -> playAgainstRobot humancolor (opponent c) newBs
        Nothing    -> pure ()

playBestMove :: Color -> BoardState -> IO (Maybe BoardState)
playBestMove c bs = do
    print "Play best move:"
    _ <- getLine
    let result = getBestMove bs c
    if isNothing result
        then pure Nothing
        else do
            let move = fromJust result
            print "Best move:"
            print move
            print ""
            let newBs = applyMove bs move
            pure $ Just newBs

playHumanMove :: Color -> BoardState -> IO (Maybe BoardState)
playHumanMove c bs = do
    print "Enter move:"
    move <- read <$> getLine
    let newBs = applyMove bs move
    pure $ Just newBs
