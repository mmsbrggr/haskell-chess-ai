{-|
Module      : Board
Description : Implementing the chess board representation 
Stability   : experimental

Implements the internal representation of a chess board as a 112 element array.
The array contains all natural board fields as well as a double border.
All fields (pieces, empty, border) are represented as small integers.
-}
module Board
    ( Field
    , Piece
    , Index
    , Color(..)
    , PieceType(..)
    , BoardState
    , initialBoardState
    , getBoardWithoutBorder
    , numberToPieceType
    , pieceTypeToNumber
    , getBoard
    , getWhitePieces
    , getBlackPieces
    )
where

import           Data.Int                       ( Int8 )
import qualified Data.Vector.Unboxed           as V

type Field = Int8
type Piece = Int8
type Index = Int8

data Color = White | Black deriving (Eq)

data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq, Enum, Bounded)

data BoardState = BoardState { getBoard       :: V.Vector Field
                             , getWhitePieces :: V.Vector (Index, Piece)
                             , getBlackPieces :: V.Vector (Index, Piece)
                             }

borderField :: Field
borderField = 8

emptyField :: Field
emptyField = 0

initialBoardState :: BoardState
initialBoardState = BoardState board (wPawns V.++ wBack) (bPawns V.++ bBack)
  where
    wBack    = backRow White
    bBack    = backRow Black
    wPawns   = pawnRow White
    bPawns   = pawnRow Black
    emptyRow = V.replicate 8 emptyField
    board    = V.concat $ addBorderFields
        [ snd $ V.unzip wBack
        , snd $ V.unzip wPawns
        , emptyRow
        , emptyRow
        , emptyRow
        , emptyRow
        , snd $ V.unzip bPawns
        , snd $ V.unzip bBack
        ]

--  | Adds two border rows on the front and to at the back, as well as
--  | one border column left and right
addBorderFields :: [V.Vector Field] -> [V.Vector Field]
addBorderFields rows =
    [borderRow, borderRow] ++ rows' ++ [borderRow, borderRow]
  where
    rows'     = map ((borderField `V.cons`) . (`V.snoc` borderField)) rows
    borderRow = V.replicate 8 borderField

backRow :: Color -> V.Vector (Index, Piece)
backRow c | c == White = V.fromList $ zip [0 ..] pieces
          | otherwise  = V.fromList $ zip [56 ..] $ map (* (-1)) pieces
  where
    pieces = map pieceTypeToNumber types
    types  = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

pawnRow :: Color -> V.Vector (Index, Piece)
pawnRow c | c == White = V.fromList $ zip [8 ..] pawns
          | otherwise  = V.fromList $ zip [48 ..] $ map (* (-1)) pawns
    where pawns = replicate 8 $ pieceTypeToNumber Pawn

-- | Piece types are represented from 1 to 7 (0 is reserved for the blank field) 
pieceTypeToNumber :: PieceType -> Piece
pieceTypeToNumber = fromIntegral . (+ 1) . fromEnum

numberToPieceType :: Piece -> PieceType
numberToPieceType n = toEnum $ fromIntegral (n - 1)

getBoardWithoutBorder :: BoardState -> V.Vector Field
getBoardWithoutBorder = V.filter (/= borderField) . getBoard
