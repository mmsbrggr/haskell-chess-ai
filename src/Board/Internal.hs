{-|
Module      : Board.Internal
Description : Implementing the chess board representation 
Stability   : experimental

Implements the internal representation of a chess board as a 112 element array.
The array contains all natural board fields as well as a double border.
All fields (pieces, empty, border) are represented as small integers.
-}
module Board.Internal where

import           Data.Int                       ( Int8 )
import qualified Data.Vector.Unboxed           as V
import           Board.Types

borderField :: Field
borderField = Field 7

emptyField :: Field
emptyField = Field 0

-- | Creates a field for a given field type, the color of the returned field is undetermined
createField :: FieldType -> Field
createField = Field . fromIntegral . fromEnum

-- | Creates a field for a given color and a given field type
createColoredField :: Color -> FieldType -> Field
createColoredField White = Field . fromIntegral . fromEnum
createColoredField Black = Field . (* (-1)) . fromIntegral . fromEnum

getFieldType :: Field -> FieldType
getFieldType (Field n) = toEnum $ fromIntegral $ abs n

getFieldColor :: Field -> Color
getFieldColor (Field n) = if n >= 0 then White else Black

getBoardWithoutBorder :: BoardState -> V.Vector Field
getBoardWithoutBorder = V.filter (/= borderField) . getBoard

getPieces :: Color -> BoardState -> V.Vector (Index, Field)
getPieces White = getWhitePieces
getPieces Black = getBlackPieces

getPiecesIndices :: Color -> BoardState -> V.Vector Index
getPiecesIndices c bs = fst $ V.unzip $ getPieces c bs

getPiecesFields :: Color -> BoardState -> V.Vector Field
getPiecesFields c bs = snd $ V.unzip $ getPieces c bs

-- | Adds a given offset to an index. It does not perform a range check due to performance reasons
changeIndexBy :: Index -> Int8 -> Index
changeIndexBy (Index i) n = Index (i + n)

-- | For a given index returns the field of the board with that index
getField :: BoardState -> Index -> Field
getField bs (Index i) = getBoard bs `V.unsafeIndex` fromIntegral i

-- | Returns true iff there is a piece on the field (so it's not empty nor a border field)
isPiece :: Field -> Bool
isPiece f = (t /= Border) && (t /= Empty) where t = getFieldType f

-- | For the proponent's color and a given field returns true iff on the field there is one of his pieces
isOwnPiece :: Color -> Field -> Bool
isOwnPiece c f = isPiece f && (getFieldColor f == c)

-- | For the proponent's color and a given field returns true iff on the field there is a piece of the opponent
isOpponentPiece :: Color -> Field -> Bool
isOpponentPiece c f = isPiece f && (getFieldColor f == opponent c)

-- | Returns the color of the opponent for a given color
opponent :: Color -> Color
opponent White = Black
opponent Black = White

-- | Returns true iff the index refers to a field in the pawn row of the player with the given color
isPawnRowIndex :: Color -> Index -> Bool
isPawnRowIndex White (Index n) = 30 < n && n < 39
isPawnRowIndex Black (Index n) = 80 < n && n < 89

-- | Returns true iff the given field is empty
isEmpty :: Field -> Bool
isEmpty = (== Empty) . getFieldType

indexToInt :: Index -> Int
indexToInt (Index n) = fromIntegral n
