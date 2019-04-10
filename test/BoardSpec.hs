module BoardSpec where

import qualified Data.Vector.Unboxed as V
import Data.Int (Int8)
import Test.Hspec
import Test.QuickCheck
import Board
import Board.Print


instance Arbitrary PieceType where
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

initialBoard :: V.Vector Field
initialBoard = V.fromList [8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,2,3,4,5,6,4,3,2,8,8,1,1,1,1,1,1,1,1,8,8,0,0,0,0,0,0,0,0,8,8,0,0,0,0,0,0,0,0,8,8,0,0,0,0,0,0,0,0,8,8,0,0,0,0,0,0,0,0,8,8,-1,-1,-1,-1,-1,-1,-1,-1,8,8,-2,-3,-4,-5,-6,-4,-3,-2,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8]

whitePieces :: V.Vector (Int8, Int8)
whitePieces = V.fromList [(8,1), (9,1), (10,1), (11,1), (12,1), (13,1), (14,1), (15,1), (0,2), (1,3), (2,4), (3,5), (4,6), (5,4), (6,3), (7,2)]

blackPieces :: V.Vector (Int8, Int8)
blackPieces = V.fromList [(48,-1), (49,-1), (50,-1), (51,-1), (52,-1), (53,-1), (54,-1), (55,-1), (56,-2), (57,-3), (58,-4), (59,-5), (60,-6), (61,-4), (62,-3), (63,-2)]

spec :: Spec
spec = do
    describe "piece type to number" $ do
        it "returns a positive number" $ property $
            \pt -> pieceTypeToNumber pt `shouldSatisfy` (> 0)

    describe "number to piece type" $ do
        it "should be the inverse of piece type to number" $ property $
            \pt -> (numberToPieceType . pieceTypeToNumber $ pt) `shouldBe` pt

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
