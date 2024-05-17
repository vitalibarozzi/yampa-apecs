{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Apecs.Yampa 
    ( cmapSF,
      cfoldSF
    )
where


import Apecs
import FRP.Yampa hiding (reactInit)
import qualified FRP.Yampa as Yampa


cmapSF :: 
    (Get w m cx, Members w m cx, Set w m cy) =>
    SF cx cy -> 
    DTime -> 
    SystemT w m ()
cmapSF sf dt = do
    cmap (_singleEmbbed sf dt) -- Todo: maybe done with a single sf handle that we call multiple times as well, with the same api, so check which is faster later


-- left-appending with the monoid. so if f is a list you get the reversed list.
cfoldSF :: 
    (Applicative f, Monoid (f cx), Members w m cx, Get w m cx) => 
    SF (f cx) b -> 
    DTime -> 
    SystemT w m b
cfoldSF sf dt = do
    fa <- cfold (\acc a -> pure a <> acc) mempty -- Todo: maybe done with a single sf handle that we call multiple times as well, with the same api, so check which is faster later
    pure (_singleEmbbed sf dt fa)


_singleEmbbed :: SF a b -> DTime -> (a -> b)
_singleEmbbed sf dt a = 
    Yampa.embed sf (a, [(dt, Nothing)]) !! 1
