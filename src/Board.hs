module Board where

import           Data.Int                       ( Int8 )
import           Data.List                      ( intercalate )
import           Data.Char                      ( toUpper
                                                , toLower
                                                )
import           Data.List.Split                ( chunksOf )
import qualified Data.Vector.Unboxed           as V

data Color = White | Black
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq, Enum, Bounded)
data BoardState = BoardState { getBoard :: (V.Vector Int8) }

instance Show PieceType
    where
    show Pawn   = "P"
    show Rook   = "R"
    show Knight = "N"
    show Bishop = "B"
    show Queen  = "Q"
    show King   = "K"

instance Show BoardState where
    show bs = unlines 
            $ (++ [letterRow])
            $ (separator :)
            $ map showRow
            $ reverse
            $ zip [1..]
            $ chunksOf 8 
            $ V.toList
            $ getBoard bs
      where
        separator = "  " ++ replicate 33 '-'
        letterRow = "    " ++ intercalate "   " ["A", "B", "C", "D", "E", "F", "G", "H"]
        showRow (n,r) = show n ++ " | " ++ intercalate " | " (map showField r) ++ " |\n" ++ separator 
        showField 0 = " "
        showField f = if f < 0
            then map toLower $ showField (-f)
            else map toUpper $ show $ numberToPieceType f

initialBoardState :: BoardState
initialBoardState = BoardState board
  where
    board = V.concat
        [ pieceRow White
        , pawnRow White
        , emptyRow
        , emptyRow
        , emptyRow
        , emptyRow
        , pawnRow Black
        , pieceRow Black
        ]

pieceRow :: Color -> V.Vector Int8
pieceRow c = V.fromList $ map ((* colorSign c) . pieceTypeToNumber) pieces
    where pieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

pawnRow :: Color -> V.Vector Int8
pawnRow c = V.map (* colorSign c) (V.replicate 8 $ pieceTypeToNumber Pawn)

emptyRow :: V.Vector Int8
emptyRow = V.replicate 8 0

colorSign :: Color -> Int8
colorSign White = 1
colorSign Black = -1

pieceTypeToNumber :: PieceType -> Int8
pieceTypeToNumber = fromIntegral . (+ 1) . fromEnum

numberToPieceType :: Int8 -> PieceType
numberToPieceType n = toEnum $ fromIntegral (n - 1)
