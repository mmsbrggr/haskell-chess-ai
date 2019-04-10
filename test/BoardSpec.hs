module BoardSpec where

import qualified Data.Vector.Unboxed as V
import Data.Int (Int8)
import Test.Hspec
import Test.QuickCheck
import Board.Internal
import Board.Print


instance Arbitrary FieldType where
    arbitrary = arbitraryBoundedEnum `suchThat` (/= Empty)

instance Arbitrary Color where
    arbitrary = arbitraryBoundedEnum

initialBoardString =
        "  ---------------------------------\n" ++
        "8 | r | n | b | q | k | b | n | r |\n" ++
        "  ---------------------------------\n" ++
        "7 | p | p | p | p | p | p | p | p |\n" ++
        "  ---------------------------------\n" ++
        "6 |   |   |   |   |   |   |   |   |\n" ++
        "  ---------------------------------\n" ++
        "5 |   |   |   |   |   |   |   |   |\n" ++
        "  ---------------------------------\n" ++
        "4 |   |   |   |   |   |   |   |   |\n" ++
        "  ---------------------------------\n" ++
        "3 |   |   |   |   |   |   |   |   |\n" ++
        "  ---------------------------------\n" ++
        "2 | P | P | P | P | P | P | P | P |\n" ++
        "  ---------------------------------\n" ++
        "1 | R | N | B | Q | K | B | N | R |\n" ++
        "  ---------------------------------\n" ++
        "    A   B   C   D   E   F   G   H\n"

mapTuple :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
mapTuple f g (x, y) = (f x, g y)

initialBoard :: V.Vector Field
initialBoard = V.fromList $ map Field [7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,2,3,4,5,6,4,3,2,7,7,1,1,1,1,1,1,1,1,7,7,0,0,0,0,0,0,0,0,7,7,0,0,0,0,0,0,0,0,7,7,0,0,0,0,0,0,0,0,7,7,0,0,0,0,0,0,0,0,7,7,-1,-1,-1,-1,-1,-1,-1,-1,7,7,-2,-3,-4,-5,-6,-4,-3,-2,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7]

whitePieces :: V.Vector (Index, Field)
whitePieces = V.fromList $ map (mapTuple Index Field) [(8,1), (9,1), (10,1), (11,1), (12,1), (13,1), (14,1), (15,1), (0,2), (1,3), (2,4), (3,5), (4,6), (5,4), (6,3), (7,2)]

blackPieces :: V.Vector (Index, Field)
blackPieces = V.fromList $ map (mapTuple Index Field) [(48,-1), (49,-1), (50,-1), (51,-1), (52,-1), (53,-1), (54,-1), (55,-1), (56,-2), (57,-3), (58,-4), (59,-5), (60,-6), (61,-4), (62,-3), (63,-2)]

spec :: Spec
spec = do
    describe "create field" $ do
        it "should be the inverse of get field type" $ property $
            \pt -> (getFieldType . createField $ pt) `shouldBe` pt

    describe "create colored field" $ do
        it "should create a field with the right color" $ property $
            \(c,pt) -> (getFieldColor $ createColoredField c pt) `shouldBe` c
    
    describe "create colored field" $ do
        it "should create a field with the right type" $ property $
            \(c,pt) -> (getFieldType $ createColoredField c pt) `shouldBe` pt
    
    describe "show initial board" $ do
        it "should return the correct string" $ do
            show initialBoardState `shouldBe` initialBoardString

    describe "board" $ do
        it "should return the correct vector" $ do
            getBoard initialBoardState `shouldBe` initialBoard

    describe "white pieces" $ do
        it "should be correct" $ do
            getWhitePieces initialBoardState `shouldBe` whitePieces 
    
    describe "black pieces" $ do
        it "should be correct" $ do
            getBlackPieces initialBoardState `shouldBe` blackPieces 
