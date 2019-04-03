module BoardSpec where

import Test.Hspec
import Test.QuickCheck

import Board

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
