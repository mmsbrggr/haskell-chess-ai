{-|
Module      : Board.Types
Description : Defines the global types for the board representation
Stability   : experimental
-}
module Board.Types where

import           Data.Int                       ( Int8 )
import           Data.Vector.Unboxed.Base       ( Unbox )
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed           as V

newtype Field = Field Int8 deriving (Eq, Enum)
derivingUnbox "Field" [t| Field -> Int8 |] [| \ (Field n) -> n |] [| \ n -> Field n |]

newtype Index = Index Int8 deriving (Eq, Enum)
derivingUnbox "Index" [t| Index -> Int8 |] [| \ (Index i) -> i |] [| \ i -> Index i |]

data Color = White | Black deriving (Show, Eq, Enum, Bounded)

data FieldType = Empty | Pawn | Rook | Knight | Bishop | Queen | King | Border
    deriving (Eq, Enum, Bounded)

data BoardState = BoardState { getBoard       :: V.Vector Field
                             , getWhitePieces :: V.Vector (Index, Field)
                             , getBlackPieces :: V.Vector (Index, Field)
                             }

data GamePhase = Opening | MiddleGame | EndGame deriving (Eq, Show)

instance Bounded Index where
    minBound = Index 0
    maxBound = Index 119
