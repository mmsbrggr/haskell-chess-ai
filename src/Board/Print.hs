module Board.Print where

import           Data.Char                      ( toUpper
                                                , toLower
                                                )
import           Data.List                      ( intercalate, splitAt, elemIndex )
import           Data.Maybe                     ( fromJust )
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
    show f = colorize $ map toUpper $ show t
      where
        t             = getFieldType f
        c             = getFieldColor f
        colorize s = if c == White then s else "\x1b[36m" ++ s ++ "\x1b[0m" 

instance Show BoardState where
    show bs = unlines [board, wPieces, bPieces]
      where
        board   = showBoard $ getBoardWithoutBorder bs
        wPieces = "White pieces: " ++ (showPieces $ getWhitePieces bs)
        bPieces = "Black pieces: " ++ (showPieces $ getBlackPieces bs)

instance Show Index where
    show (Index i) = map toLower $ (columns !! c) ++ (rows !! r)
      where
        c       = (fromIntegral i) `mod` 10
        r       = (fromIntegral i) `div` 10

instance Read Index where
    readsPrec _ string = [(Index i, "")]
        where 
            (c, r) = splitAt 1 string
            c'     = fromJust $ elemIndex (map toUpper c) columns
            r'     = fromJust $ elemIndex (map toUpper r) rows
            i      = fromIntegral $ (r' * 10) + c' 
            
columns :: [String]
columns = ["a", "A", "B", "C", "D", "E", "F", "G", "H", "h"]

rows :: [String]
rows = ["-1", "-0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

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
