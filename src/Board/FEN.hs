module Board.FEN
    ( getBoardStateFromFEN
    , initialBoardState
    )
where

import           Board.Internal
import           Board.Types
import qualified Data.Vector.Unboxed           as V
import           Data.List.Split
import           Data.Char                      ( isDigit
                                                , digitToInt
                                                )

initialBoardState :: BoardState
initialBoardState =
    getBoardStateFromFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

getBoardStateFromFEN :: String -> BoardState
getBoardStateFromFEN fen = BoardState board whitePieces blackPieces
  where
    fen'        = head $ splitOn " " fen -- Take only the board 
    board       = getBoardFromFEN fen'
    whitePieces = getPiecesFromBoard White board
    blackPieces = getPiecesFromBoard Black board

getBoardFromFEN :: String -> V.Vector Field
getBoardFromFEN =
    V.concat . addBorderFields . reverse . map createRow . splitOn "/"

getPiecesFromBoard :: Color -> V.Vector Field -> V.Vector (Index, Field)
getPiecesFromBoard c board = V.filter (pieces c) $ V.zip indices board
  where
    pieces c (_, f) = isOwnPiece c f
    indices = V.fromList [(Index 0) .. (Index 119)]

createRow :: String -> V.Vector Field
createRow = V.fromList . concat . map createFields

createFields :: Char -> [Field]
createFields c = case c of
    'p' -> [createColoredField Black Pawn]
    'r' -> [createColoredField Black Rook]
    'n' -> [createColoredField Black Knight]
    'b' -> [createColoredField Black Bishop]
    'q' -> [createColoredField Black Queen]
    'k' -> [createColoredField Black King]
    'P' -> [createColoredField White Pawn]
    'R' -> [createColoredField White Rook]
    'N' -> [createColoredField White Knight]
    'B' -> [createColoredField White Bishop]
    'Q' -> [createColoredField White Queen]
    'K' -> [createColoredField White King]
    d   -> if isDigit d then replicate (digitToInt d) emptyField else []

--  | Adds two border rows on the front and to at the back, as well as
--  | one border column left and right
addBorderFields :: [V.Vector Field] -> [V.Vector Field]
addBorderFields rows = map
    ((borderField `V.cons`) . (`V.snoc` borderField))
    rows'
  where
    rows'     = [borderRow, borderRow] ++ rows ++ [borderRow, borderRow]
    borderRow = V.replicate 8 borderField
