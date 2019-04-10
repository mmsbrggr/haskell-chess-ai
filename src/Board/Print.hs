module Board.Print where

import           Data.Char                      ( toUpper
                                                , toLower
                                                )
import           Data.List                      ( intercalate )
import           Data.List.Split                ( chunksOf )
import qualified Data.Vector.Generic           as V
import           Board

instance Show PieceType where
    show Pawn   = "P"
    show Rook   = "R"
    show Knight = "N"
    show Bishop = "B"
    show Queen  = "Q"
    show King   = "K"

instance Show BoardState where
    show bs =
        unlines
            $ (++ [letterRow])
            $ (separator :)
            $ map showRow
            $ reverse
            $ zip [1 ..]
            $ chunksOf 8
            $ V.toList
            $ getBoardWithoutBorder bs
      where
        separator = "  " ++ replicate 33 '-'
        letterRow =
            "    " ++ intercalate "   " ["A", "B", "C", "D", "E", "F", "G", "H"]
        showRow (n, r) =
            show n
                ++ " | "
                ++ intercalate " | " (map showField r)
                ++ " |\n"
                ++ separator
        showField 0 = " "
        showField f = if f < 0
            then map toLower $ showField (-f)
            else map toUpper $ show $ numberToPieceType f
