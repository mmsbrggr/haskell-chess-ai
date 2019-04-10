module Moves where

import Data.Vector.Unboxed.Base (Unbox)
import Data.Vector.Unboxed.Deriving
import Board

newtype Move = Move (Index, Index)
derivingUnbox "Move"
    [t| Move -> (Index, Index) |]
    [| \ (Move (from, to)) -> (from, to) |]
    [| \ (from, to) -> (Move (from, to)) |]

generateMoves :: BoardState -> Color -> [Move]
generateMoves = undefined
