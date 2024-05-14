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
    ( CHandle,
      newCHandle,
      cmapHandle,
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


-----------------------------------------------------------
-- | A "component handle" to be used to react in the System monad.
newtype CHandle cx cy 
    = CHandle {- PRIVATE CONSTRUCTOR -} 
          (IORef cy, ReactHandle (Event (SF cx cy)) cy)
instance Show (CHandle cx cy) where
    show _ = "CHandle(...)"


-----------------------------------------------------------
-- | Constructor for the component handle.
newCHandle :: (MonadIO m) => SystemT w m (CHandle cx cy)
{-# INLINABLE newCHandle #-}
newCHandle =
    liftIO do
        ref    <- newIORef (let x = x in x)
        handle <- reactInit (pure NoEvent) (actuate ref) switcher
        return (CHandle (ref, handle))
  where
    actuate ref handle updated pos = do
        when updated (writeIORef ref pos) 
        pure updated
    switcher = proc ev -> drSwitch (constant (let x = x in x)) -< ((let x = x in x), ev)


-----------------------------------------------------------
-- | Consumer of the component handle.
cmapHandle :: 
    ( MonadIO   m
    , Members w m cx
    , Get     w m cx
    , Set     w m cy
    ) =>
    CHandle cx cy ->
    (DTime, cx -> SF cx cy) -> 
    SystemT w m ()
{-# INLINABLE cmapHandle #-}
cmapHandle (CHandle (ref, handle)) (dt, fsf) = do
    cmapM 
        $ \components -> do
            liftIO do
                -- TODO this is need to make it work, not sure why. find out probably because of the
                -- switch with delay
                _ <- react handle (0, Just (Event (fsf components)))
                _ <- react handle (dt, Nothing)
                readIORef ref

-- TODO convert the other functions like cfold
