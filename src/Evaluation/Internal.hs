{-|
Module      : Evaluation.Internal
Description : Implements a simple board evaluation 
Stability   : experimental

Implements one of the simplest possible board evaluations.
The value of a board is just the sum of its piece vlaues.
The white pieces are considered positive, the black pieces negative.
-}
module Evaluation.Internal where

import qualified Data.Vector.Unboxed           as V
import           Board
import           Evaluation.Tables

fieldTypeValue :: FieldType -> Int
fieldTypeValue ft = case ft of
    Pawn   -> 100
    Rook   -> 500
    Knight -> 320
    Bishop -> 330
    Queen  -> 900
    King   -> 20000
    _      -> 0

valueOf :: BoardState -> GamePhase -> Int
valueOf bs gp = valueOfColor bs gp White - valueOfColor bs gp Black

valueOfColor :: BoardState -> GamePhase -> Color -> Int
valueOfColor bs gp c = V.foldr ((+) . valueOfField gp c) 0 $ getPieces c bs

valueOfField :: GamePhase -> Color -> (Index, Field) -> Int
valueOfField gp c (i, f) = fieldTypeValue ft + positionalCorrection gp c ft i
    where ft = getFieldType f

positionalCorrection :: GamePhase -> Color -> FieldType -> Index -> Int
positionalCorrection gp c ft i = getTable gp c ft V.! indexToInt i

getTable :: GamePhase -> Color -> FieldType -> V.Vector Int
getTable gp c ft = case ft of
    Pawn   -> pawn c
    Rook   -> rook c
    Knight -> knight c
    Bishop -> bishop c
    Queen  -> queen c
    King   -> if gp == EndGame then kingEnd c else kingMiddle c
    _      -> V.replicate 120 0

isMaximizing :: Color -> Bool
isMaximizing White = True
isMaximizing Black = False
