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
import           Data.Vector.Unboxed.Base       ( Unbox )
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed           as V

newtype Field = Field Int8 deriving (Eq, Enum)
derivingUnbox "Field" [t| Field -> Int8 |] [| \ (Field n) -> n |] [| \ n -> Field n |]


newtype Index = Index Int8 deriving (Eq, Enum, Show)
derivingUnbox "Index" [t| Index -> Int8 |] [| \ (Index i) -> i |] [| \ i -> Index i |]

data Color = White | Black deriving (Show, Eq, Enum, Bounded)

data FieldType = Empty | Pawn | Rook | Knight | Bishop | Queen | King | Border
    deriving (Eq, Enum, Bounded)

data BoardState = BoardState { getBoard       :: V.Vector Field
                             , getWhitePieces :: V.Vector (Index, Field)
                             , getBlackPieces :: V.Vector (Index, Field)
                             }

instance Bounded Field where
    minBound = createColoredField Black (maxBound :: FieldType)
    maxBound = createColoredField White (maxBound :: FieldType) 

instance Bounded Index where
    minBound = Index 0
    maxBound = Index 119 

borderField :: Field
borderField = Field 7

emptyField :: Field
emptyField = Field 0

initialBoardState :: BoardState
initialBoardState = BoardState board (wPawns V.++ wBack) (bPawns V.++ bBack)
  where
    wBack    = backRow White
    bBack    = backRow Black
    wPawns   = pawnRow White
    bPawns   = pawnRow Black
    emptyRow = V.replicate 8 emptyField
    board    = V.concat $ addBorderFields
        [ snd $ V.unzip wBack
        , snd $ V.unzip wPawns
        , emptyRow
        , emptyRow
        , emptyRow
        , emptyRow
        , snd $ V.unzip bPawns
        , snd $ V.unzip bBack
        ]

--  | Adds two border rows on the front and to at the back, as well as
--  | one border column left and right
addBorderFields :: [V.Vector Field] -> [V.Vector Field]
addBorderFields rows = map ((borderField `V.cons`) . (`V.snoc` borderField)) rows'
    where
        rows' = [borderRow, borderRow] ++ rows ++ [borderRow, borderRow]
        borderRow = V.replicate 8 borderField

-- | Returns the back row for a given color together with the indices the peices stand on
backRow :: Color -> V.Vector (Index, Field)
backRow c = V.fromList $ zip (indices c) pieces
  where
    pieces = map (createColoredField c) types
    types  = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    indices White = [(Index 0) ..]
    indices Black = [(Index 56) ..]

-- | Returns the pawn row for a given color together with the indices the peices stand on
pawnRow :: Color -> V.Vector (Index, Field)
pawnRow c = V.fromList $ zip (indices c) pawns
  where
    pawns = replicate 8 $ createColoredField c Pawn
    indices White = [(Index 8) ..]
    indices Black = [(Index 48) ..]

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
