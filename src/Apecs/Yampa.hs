{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Apecs.Yampa 
    ( cmapSF,
      reactInitSF,
      ApecsHandle,
    )
where


import Apecs
import FRP.Yampa hiding (reactInit)
import qualified FRP.Yampa as Yampa
import Control.Monad.IO.Class
import Control.Monad
import Data.IORef


-----------------------------------------------------------
type ApecsHandle cx cy = ReactHandle (cx, Event (SF cx cy)) (IORef cy, cy)


-----------------------------------------------------------
reactInitSF :: (MonadIO m) => SF cx cy -> cx -> SystemT w m (IORef cy, ApecsHandle cx cy)
reactInitSF sf cx = do
    ref <- liftIO $ newIORef undefined
    handle <- liftIO $ Yampa.reactInit (pure (cx, NoEvent)) (\_ updated (ref_,cy) -> when updated (writeIORef ref_ cy) >> pure updated) (pure ref &&& rSwitch sf)
    return (ref, handle)


-----------------------------------------------------------
cmapSF :: 
    forall w m cx cy. 
    ( Members w m cx
    , Members w m cy
    , Members w m (IORef cy)
    , Members w m (ApecsHandle cx cy)
    , Get w m (IORef cy)
    , Get w m (ApecsHandle cx cy)
    , Get w m cx
    , Get w m cy
    , Set w m cy
    , MonadIO m
    ) => 
    (DTime, Event (SF cx cy)) -> 
    SystemT w m ()
cmapSF (dt,esf) = do
    cmapM 
        (\( hdl :: ReactHandle (cx, Event (SF cx cy)) (IORef cy, cy)
          , ref :: IORef cy
          , cx  :: cx
          , cy  :: cy
          ) -> 
        do updated <- liftIO (react hdl (dt, Just (cx, esf)))
           if updated 
               then liftIO (readIORef ref) 
               else pure cy)
