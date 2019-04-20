{-|
Module      : Moves.Internal
Description : Implements movements on the chess board representation 
Stability   : experimental

Implements representation, generation and application of moves on the
chess board representation. This module is highly dependent on the internal
representation of the board.
TODOs: En passent capture, casteling, promotions
-}
module Moves.Internal where

import qualified Data.Vector.Unboxed           as V
import qualified Utils                         as U
import           Data.Int                       ( Int8 )
import           Data.Vector.Unboxed.Base       ( Unbox )
import           Data.Vector.Unboxed.Deriving
import           Board
import           Board.Types

newtype Move = Move (Index, Index) deriving newtype (Show)
derivingUnbox "Move"
    [t| Move -> (Index, Index) |]
    [| \ (Move (from, to)) -> (from, to) |]
    [| \ (from, to) -> Move (from, to) |]

-- | Succinct type for the movement state:
-- | Index of moved piece, color of moved piece and the state of the board
type MoveState = (Index, Color, BoardState)

-- | This type represents a shift of an index
type Offset = Int8

data MoveType = Jump | Slide

applyMove :: BoardState -> Move -> BoardState
applyMove bs (Move (from, to)) = BoardState board wPieces bPieces
    where board     = V.unsafeUpd (getBoard bs) [(indexToInt from, emptyField), (indexToInt to, fromField)] 
          wPieces   = constructPieces White 
          bPieces   = constructPieces Black
          fromField = getField bs from
          toField   = getField bs to
          player    = getFieldColor fromField
          constructPieces c = if player == c
                                 then U.change (getPieces c bs) (from, fromField) (to, fromField)
                                 else if isOwnPiece c toField 
                                 then U.remove (to, toField) (getPieces c bs)
                                 else getPieces c bs 

-- | Returns all possible moves for a given color and a given board state
getAllMoves :: Color -> BoardState -> V.Vector Move
getAllMoves c bs = V.concatMap getMoves' $ getPiecesIndices c bs
    where getMoves' i = getMoves (i, c, bs)

-- | Returns all possible moves of a single piece
getMoves :: MoveState -> V.Vector Move 
getMoves ms =
    let (movetype, offsets) = getMoveInfo ms in
    case movetype of
      Jump  -> getJumpMoves ms offsets
      Slide -> getSlideMoves ms offsets

-- | Returns all moves, constructed from the index in the move state by adding
-- | the possible offsets to this index
getJumpMoves :: MoveState -> V.Vector Offset -> V.Vector Move
getJumpMoves ms@(i, _, _) offsets=
    V.map (\dest -> Move (i, dest))
    $ V.filter (not . isIllegalDestination ms) 
    $ V.map (changeIndexBy i) offsets


-- | Returns all moves, constructed from the index in the move state by "sliding" the index
-- | across the board in the direction of the offsets
getSlideMoves :: MoveState -> V.Vector Offset -> V.Vector Move
getSlideMoves ms offsets = V.concatMap (getSingleSlideMoves ms) offsets 

-- | Returns all moves, constructed from the index in the move state by "sliding" the index
-- | across the board in the direction of the single offset
getSingleSlideMoves :: MoveState -> Offset -> V.Vector Move
getSingleSlideMoves ms@(i, _, _) o =
    V.map (\dest -> Move (i, dest))
    $ snd . V.unzip . V.unsafeTail
    $ V.takeWhile isPossibleSlide' destinations 
        where destinations     = getPossibleSlidesFrom i o
              isPossibleSlide' = isPossibleSlide ms destinations

-- | For a move state, all potential sliding destinations (row, column or diagonal) and
-- | a given potential destination, the function returns true iff the destination is legal.
-- | Result is only correct if all destinations previous to the passed one are possible.
isPossibleSlide :: MoveState -> V.Vector (Int, Index) -> (Int, Index) -> Bool
isPossibleSlide ms@(_, c, bs) destinations (n, dest)
    | n == 0    = True
    | otherwise = not $ isIllegalDestination ms dest || isOpponentPiece c lastField
    where lastField = getField bs $ snd $ V.unsafeIndex destinations (n-1) 

-- | Constructs all possible slide destinations from an index into the direction of an offset
getPossibleSlidesFrom :: Index -> Offset -> V.Vector (Int, Index)
getPossibleSlidesFrom i o = V.constructN 10 iterator 
    where iterator v = if V.null v
                          then (0, i)
                          else let (n, j) = V.last v
                                in (n + 1, changeIndexBy j o)

-- | Returns information about a move
getMoveInfo :: MoveState -> (MoveType, V.Vector Offset)
getMoveInfo ms@(i, c, bs) =
    let fieldtype = getFieldType $ getField bs i in
        case fieldtype of
            Pawn   -> (Jump,  getPawnOffsets ms)
            Rook   -> (Slide, V.fromList [10, -10, 1, -1])
            Knight -> (Jump,  V.fromList [8, -8, 12, -12, 19, -19, 21, -21])
            Bishop -> (Slide, V.fromList [9, -9, 11, -11])
            Queen  -> (Slide, V.fromList [10, -10, 1, -1, 11, -11, 9, -9])
            King   -> (Jump,  V.fromList [10, -10, 1, -1, 11, -11, 9, -9])
            _      -> (Jump,  V.empty)

-- | Get the possible offsets for a pawn (the index in the move state has to refer to a pawn)
getPawnOffsets :: MoveState -> V.Vector Offset
getPawnOffsets (i, c, bs) = V.force $ V.filter (/= notPossible) $ V.generate 4 gen
    where notPossible = 0
          colorize j = if c == White then j else j * (-1)
          front  = getField bs $ changeIndexBy i (colorize 10)
          front2 = getField bs $ changeIndexBy i (colorize 20)
          diagL  = getField bs $ changeIndexBy i (colorize 9)
          diagR  = getField bs $ changeIndexBy i (colorize 11)
          gen 0 = if isEmpty front then colorize 10 else notPossible
          gen 1 = if isPawnRowIndex c i && isEmpty front && isEmpty front2 then colorize 20 else notPossible 
          gen 2 = if isOpponentPiece c diagL then colorize 9 else notPossible 
          gen 3 = if isOpponentPiece c diagR then colorize 11 else notPossible

-- | A destination is illegal if it is outside the board or contains one of the player's pieces
isIllegalDestination :: MoveState -> Index -> Bool
isIllegalDestination (_, c, bs) dest = getFieldType f == Border || isOwnPiece c f 
    where f = getField bs dest
