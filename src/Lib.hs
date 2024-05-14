{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE Arrows     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Lib 
    ( SHandle,
      reactive,
      cmapSF,
    )
where


import Apecs
import qualified Apecs.Core
import FRP.Yampa
import Linear.V2
import Control.Concurrent
import Control.Monad
import Debug.Trace
import Control.Monad.IO.Class
import GHC.Generics
import Data.TreeDiff.Class
import Data.TreeDiff.Pretty
import Data.IORef
import Control.Concurrent.MVar
import Linear    as L
import Unsafe.Coerce


-----------------------------------------------------------
-- | A handle to be used to react in the System monad.
newtype SHandle a b 
    = SHandle {- PRIVATE CONSTRUCTOR -} 
          (IORef b, ReactHandle (Event (SF a b)) b)


-----------------------------------------------------------
-- | Constructor for the SHandle.
reactive ::
    (MonadIO m) =>
    (a -> SF a b) ->
    a ->
    b ->
    SystemT w m (SHandle a b)
{-# INLINABLE reactive #-}
reactive initSF initialState initialInput =
    liftIO do
        ref    <- newIORef initialInput
        handle <- reactInit (pure NoEvent) (actuate ref) switcher
        return (SHandle (ref, unsafeCoerce handle))
  where
    actuate ref handle updated pos = do
        when updated (writeIORef ref pos) 
        pure updated
    switcher = proc ev -> drSwitch (initSF initialState) -< (initialState, ev)

-----------------------------------------------------------
-- | Consumer of the SHandle.
cmapSF :: 
    (MonadIO m, Get w m cx, Members w m cx, Set w m cy)=>
    SHandle cx cy ->
    DTime -> 
    (cx -> SF cx cy) -> 
    SystemT w m ()
{-# INLINABLE cmapSF #-}
cmapSF (SHandle (ref, handle)) dt fsf = do
    cmapM 
        $ \components -> do
            liftIO do
                -- TODO this is need to make it work, not sure why. find out probably because of the
                -- switch with delay
                _ <- react handle (0, Just (Event (fsf components)))
                _ <- react handle (dt, Nothing)
                readIORef ref

-- TODO convert the other functions like cfold
