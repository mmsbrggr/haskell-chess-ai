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
    , getFieldType
    , getFieldColor
    , changeIndexBy
    , getField
    , isOwnPiece
    , isOpponentPiece
    , isPawnRowIndex
    , isEmpty
    , opponent
    )
where

import Board.Internal
import Board.Types
