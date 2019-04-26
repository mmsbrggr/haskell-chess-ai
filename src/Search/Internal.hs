-- | TODO: This was a evening hack. This could be cleaned up with a GameTree etc. (but it works)

module Search.Internal where

import           Debug.Trace

import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe                     ( isJust )
import qualified Data.Vector.Unboxed           as V
import           Utils
import           Board
import           Moves
import           Evaluation

newtype Alpha = Alpha Int deriving (Eq, Ord)
newtype Beta  = Beta Int deriving (Eq, Ord)

type SearchState = (Alpha, Beta)
type SearchResult = Maybe (Int, Move)
type Depth = Int

depthLimit :: Depth
depthLimit = 4

sortMoves :: BoardState -> V.Vector Move -> V.Vector Move
sortMoves bs = V.modify (sortOn captureValue)
  where
    captureValue m = fieldTypeValue $ getFieldType $ getField bs $ getTo m

gamePhase :: BoardState -> GamePhase
gamePhase bs = if isSmall White && isSmall Black then EndGame else MiddleGame
  where
    isSmall c =
        not (hasQueen c)
            || V.null (pieces c)
            || ((V.length (pieces c) == 1) && isMinor (V.head $ pieces c))
    hasQueen c =
        isJust $ V.find ((==) Queen . getFieldType) (getPiecesFields c bs)
    pieces c = V.filter ((`elem` [Knight, Bishop, Rook]) . getFieldType)
                        (getPiecesFields c bs)
    isMinor = (`elem` [Knight, Bishop]) . getFieldType

initialSearchState :: SearchState
initialSearchState = (Alpha minBound, Beta maxBound)

getBestMove :: BoardState -> Color -> Maybe Move
getBestMove bs c =
    let gp = gamePhase bs
        (result, _) =
                runState (alphaBeta bs gp c depthLimit) initialSearchState
    in  maybe Nothing (Just . snd) result

getAlpha :: MonadState SearchState m => m Alpha
getAlpha = gets fst

getBeta :: MonadState SearchState m => m Beta
getBeta = gets snd

setAlpha :: MonadState SearchState m => Alpha -> m ()
setAlpha a = do
    old <- getAlpha
    if a > old then getBeta >>= put . (a, ) else pure ()

setBeta :: MonadState SearchState m => Beta -> m ()
setBeta b = do
    old <- getBeta
    if b < old then getAlpha >>= put . (, b) else pure ()

alphaBeta
    :: MonadState SearchState m
    => BoardState
    -> GamePhase
    -> Color
    -> Depth
    -> m SearchResult
alphaBeta bs gp _ 0 = pure $ Nothing 
alphaBeta bs gp c d = do
    (Alpha a) <- getAlpha
    (Beta  b) <- getBeta
    if a >= b
        then pure Nothing -- CUTOFF
        else do
            let moves = sortMoves bs $ getAllMoves c bs
            if V.null moves then pure Nothing else alphaBetaBody bs gp c d moves

alphaBetaBody
    :: MonadState SearchState m
    => BoardState
    -> GamePhase
    -> Color
    -> Depth
    -> V.Vector Move
    -> m SearchResult
alphaBetaBody bs gp c d moves = do
    stateOld <- get
    result   <- V.foldM' (alphaBetaStep bs gp c d) Nothing moves
    put stateOld
    let baseCase = V.foldr (baseStep bs gp c) Nothing moves
    pure $ maybe baseCase Just result

alphaBetaStep
    :: MonadState SearchState m
    => BoardState
    -> GamePhase
    -> Color
    -> Depth
    -> SearchResult
    -> Move
    -> m SearchResult
alphaBetaStep bs gp c d accumulator move = do
    let child = applyMove bs move
    recRes <- alphaBeta child gp (opponent c) (d - 1)
    case recRes of
        Nothing         -> pure accumulator
        Just (value, _) -> if isNothing accumulator || cmp value accumulator
            then do
                setLimit value
                pure $ Just (value, move)
            else pure accumulator
  where
    cmp v (Just (a, _)) = if isMaximizing c then v > a else v < a
    setLimit l =
        if isMaximizing c then setAlpha (Alpha l) else setBeta (Beta l)

baseStep
    :: BoardState -> GamePhase -> Color -> Move -> SearchResult -> SearchResult
baseStep bs gp c move accumulator =
    if isNothing accumulator || cmp value accumulator
        then Just (value, move) 
        else accumulator
  where
    child = applyMove bs move
    value = valueOf child gp
    cmp v (Just (a, _)) = if isMaximizing c then v > a else v < a
