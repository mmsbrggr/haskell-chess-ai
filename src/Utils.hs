module Utils
    ( remove
    , change
    )
where

import qualified Data.Vector.Generic           as G

remove :: (G.Vector v a, Eq a) => a -> v a -> v a
remove el v = case G.elemIndex el v of
    Just i  -> let (f, b) = G.splitAt i v in f G.++ (G.tail b)
    Nothing -> v

change :: (G.Vector v a, Eq a) => v a -> a -> a -> v a
change v old new = case G.elemIndex old v of
    Just i  -> G.unsafeUpd v [(i, new)]
    Nothing -> v
