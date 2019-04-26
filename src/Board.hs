module Board
    ( Field
    , Index
    , Color(..)
    , FieldType(..)
    , GamePhase(..)
    , BoardState
    , emptyField
    , initialBoardState
    , getBoardWithoutBorder
    , getPieces
    , getPiecesIndices
    , getPiecesFields
    , getFieldType
    , getFieldColor
    , changeIndexBy
    , getField
    , isOwnPiece
    , isOpponentPiece
    , isPawnRowIndex
    , isEmpty
    , opponent
    , indexToInt
    , getBoardStateFromFEN
    )
where

import Board.Internal
import Board.Types
import Board.Print
import Board.FEN
