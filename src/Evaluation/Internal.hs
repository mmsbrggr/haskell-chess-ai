module Evaluation.Internal where

import qualified Data.Vector.Unboxed           as V
import           Board

fieldTypeValue :: FieldType -> Int
fieldTypeValue ft = case ft of
    Pawn   -> 1
    Rook   -> 5
    Knight -> 3
    Bishop -> 3
    Queen  -> 9
    King   -> 1000
    _      -> 0

valueOf :: BoardState -> Int
valueOf bs = (valueOfColor White) - (valueOfColor Black)
  where
    valueOfColor c =
        V.foldr (\f -> (+) (fieldTypeValue $ getFieldType f)) 0
            $ getPiecesFields c bs
