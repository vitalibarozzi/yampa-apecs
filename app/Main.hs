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

module Main (main) where

import Lib
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
newtype Position = Position (V2 Double) deriving Show


-----------------------------------------------------------
newtype Velocity = Velocity (V2 Double) deriving Show


-----------------------------------------------------------
data Flying = Flying


-----------------------------------------------------------
makeWorldAndComponents "Asteroids" [''Position, ''Velocity, ''Flying]


-----------------------------------------------------------
main :: IO ()
main = do
    asteroids <- initAsteroids
    runSystem app asteroids
  where
    app = do
        -- TODO how the backends fit in this situation here?
        -- brickHdl <- liftIO $ reactInitBrick ...
        -- cfold ... \x -> liftIO $ react brickHdl (0.1, x) 
        posHdl <- newCHandle
        newEntity (Position 0 , Velocity 1)
        newEntity (Position 50, Velocity (-1))
        cmapHandle posHdl (10, positionSF)
        newEntity (Position 100, Velocity (-1))
        cmapHandle posHdl (10, positionSF)
        (cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p))

    positionSF :: (Position, Velocity) -> SF a Position 
    positionSF (Position x0, Velocity v0) =
        proc input -> do 
            x1 <- (arr (+x0) <<< integral) -< v0
            returnA -< Position x1


-----------------------------------------------------------
instance (Eq a, Floating a) => VectorSpace (V2 a) a where
  zeroVector = L.zero
  (*^) = (L.*^)
  --(^) = (L.^)
  negateVector = L.negated
  (^+^) = (L.^+^)
  (^-^) = (L.^-^)
  dot = L.dot
