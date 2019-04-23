module Board.Print where

import           Data.Char                      ( toUpper
                                                , toLower
                                                )
import           Data.List                      ( intercalate )
import           Data.List.Split                ( chunksOf )
import qualified Data.Vector.Unboxed           as V
import           Board.Internal
import           Board.Types

instance Show FieldType where
    show Empty  = " "
    show Border = "X"
    show Pawn   = "P"
    show Rook   = "R"
    show Knight = "N"
    show Bishop = "B"
    show Queen  = "Q"
    show King   = "K"

instance Show Field where
    show f = transformator $ show t
      where
        t             = getFieldType f
        c             = getFieldColor f
        transformator = if c == White then map toUpper else map toLower

instance Show BoardState where
    show bs = unlines [board, wPieces, bPieces]
      where
        board   = showBoard $ getBoardWithoutBorder bs
        wPieces = "White pieces: " ++ (showPieces $ getWhitePieces bs)
        bPieces = "Black pieces: " ++ (showPieces $ getBlackPieces bs)

instance Show Index where
    show (Index i) = (columns !! c) ++ (rows !! r)
      where
        columns = ["a", "A", "B", "C", "D", "E", "F", "G", "H", "h"]
        rows = ["-1", "-0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
        c       = (fromIntegral i) `mod` 10
        r       = (fromIntegral i) `div` 10

showBoard :: V.Vector Field -> String
showBoard board =
    unlines
        $ (++ [letterRow])
        $ (separator :)
        $ map showRow
        $ reverse
        $ zip [1 ..]
        $ chunksOf 8
        $ V.toList
        $ board
  where
    separator = "  " ++ replicate 33 '-'
    letterRow =
        "    " ++ intercalate "   " ["A", "B", "C", "D", "E", "F", "G", "H"]
    showRow (n, r) =
        show n ++ " | " ++ intercalate " | " (map show r) ++ " |\n" ++ separator

showPieces :: V.Vector (Index, Field) -> String
showPieces = intercalate " " . map show . V.toList
