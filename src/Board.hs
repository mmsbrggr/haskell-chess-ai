module Board
    ( Field
    , Index
    , Color(..)
    , FieldType(..)
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
    )
where

import Board.Internal
import Board.Types
import Board.Print
