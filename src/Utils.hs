module Utils
    ( remove
    , change
    , sortOn
    )
where

import qualified Data.Vector.Generic           as G
import           Control.Monad.Primitive
import           Data.Vector.Generic.Mutable.Base
import           Data.Vector.Algorithms.Merge     ( sortBy )

remove :: (G.Vector v a, Eq a) => a -> v a -> v a
remove el v = case G.elemIndex el v of
    Just i  -> let (f, b) = G.splitAt i v in f G.++ (G.tail b)
    Nothing -> v

change :: (G.Vector v a, Eq a) => v a -> a -> a -> v a
change v old new = case G.elemIndex old v of
    Just i  -> G.unsafeUpd v [(i, new)]
    Nothing -> v

sortOn :: (PrimMonad m, MVector v a, Ord b) => (a -> b) -> v (PrimState m) a -> m ()
sortOn f = sortBy comparing 
    where comparing x y = (flip compare) (f x) (f y)
