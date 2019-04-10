module Board
    ( Field
    , Index
    , Color(..)
    , FieldType(..)
    , BoardState
    , initialBoardState
    , getBoardWithoutBorder
    , getBoard
    , getWhitePieces
    , getBlackPieces
    , getFieldType
    , getFieldColor
    )
where

import Board.Internal
