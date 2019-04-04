module Board where

import           Data.Int                       ( Int8 )
import qualified Data.Vector.Unboxed           as V

type Field = Int8
type Piece = Int8
type Index = Int8

data Color = White | Black deriving (Eq)

data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq, Enum, Bounded)

data BoardState = BoardState { getBoard       :: (V.Vector Field)
                             , getWhitePieces :: (V.Vector (Index, Piece))
                             , getBlackPieces :: (V.Vector (Index, Piece))
                             }

initialBoardState :: BoardState
initialBoardState = BoardState board (wPawns V.++ wBack) (bPawns V.++ bBack) 
  where
    wBack  = backRow White
    bBack  = backRow Black
    wPawns = pawnRow White
    bPawns = pawnRow Black
    board = V.concat
        [ snd $ V.unzip wBack 
        , snd $ V.unzip wPawns 
        , emptyRow
        , emptyRow
        , emptyRow
        , emptyRow
        , snd $ V.unzip bPawns 
        , snd $ V.unzip bBack
        ]

backRow :: Color -> V.Vector (Index, Piece)
backRow c | c == White = V.fromList $ zip [0 ..] pieces
           | otherwise  = V.fromList $ zip [56 ..] $ map (* (-1)) pieces
  where
    pieces = map pieceTypeToNumber types
    types  = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

pawnRow :: Color -> V.Vector (Index, Piece)
pawnRow c | c == White = V.fromList $ zip [8 ..] pawns
          | otherwise  = V.fromList $ zip [48..] $ map (* (-1)) pawns
    where pawns = replicate 8 $ pieceTypeToNumber Pawn

emptyRow :: V.Vector Field
emptyRow = V.replicate 8 0

pieceTypeToNumber :: PieceType -> Piece
pieceTypeToNumber = fromIntegral . (+ 1) . fromEnum

numberToPieceType :: Piece -> PieceType
numberToPieceType n = toEnum $ fromIntegral (n - 1)
