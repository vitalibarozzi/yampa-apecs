{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE BangPatterns     #-}
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
-- | Internal type.
newtype ApecsHandle cx cy = ApecsHandle 
    ( IORef cy
    , ReactHandle (cx, Event (SF cx cy)) cy
    )


-----------------------------------------------------------
-- | Constructor.
reactInitSF :: (MonadIO m) => SF cx cy -> cx -> SystemT w m (ApecsHandle cx cy)
{-# INLINE reactInitSF #-}
reactInitSF !sf cx = do
    liftIO do
        !ref <- newIORef (error "never called")
        !hdl <- Yampa.reactInit (pure (cx, Event sf)) (actuate ref) (drSwitch sf)
        return (ApecsHandle (ref, hdl))
  where
    actuate !ref _ !updated cy = do
        when updated (writeIORef ref cy) 
        pure updated


-----------------------------------------------------------
-- | Use an SF to map all entities that have the components
-- cx and cy associated with a handler.
cmapSF :: 
    forall w m cx cy. 

    ( Members w m cx
    , Get     w m cx

    , Members w m cy
    , Get     w m cy
    , Set     w m cy

    , Members w m (ApecsHandle cx cy)
    , Get     w m (ApecsHandle cx cy)
    , MonadIO m
    ) => 
    DTime -> 
    SF cx cy -> 
    SystemT w m ()
{-# INLINABLE cmapSF #-}
cmapSF dt sf = do
    cmapM 
        \( ApecsHandle (ref, hdl) :: ApecsHandle cx cy
         , cx                     :: cx
         , cy                     :: cy
         ) -> 
        liftIO do
            updated1 <- react hdl (0, Just (cx, Event sf))
            if updated1
               then do
                    !updated2 <- react hdl (dt, Nothing)
                    if updated2
                       then readIORef ref
                       else pure cy
               else pure cy
