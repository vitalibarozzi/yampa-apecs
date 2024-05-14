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


newtype Position = Position (V2 Double) deriving Show
newtype Velocity = Velocity (V2 Double) deriving Show

data Flying = Flying

makeWorldAndComponents "Asteroids" [''Position, ''Velocity, ''Flying]


-----------------------------------------------------------
main :: IO ()
main = do
    asteroids <- initAsteroids
    runSystem app asteroids
  where
    app = do
        newEntity (Position 0, Velocity 1)
        newEntity (Position 2, Velocity 1)
        newEntity (Position 1, Velocity 2)

        cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p)

        h <- reactive position (Position 0, Velocity 0) (Position 0) 

        --forever (myGame h >> (cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p)))

        myGame h >> (cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p))
        newEntity (Position 100, Velocity 1)

        myGame h >> (cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p))
        newEntity (Position 10, Velocity 1)

        myGame h >> (cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p))
        myGame h >> (cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p))
        myGame h >> (cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p))
        myGame h >> (cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p))
    myGame :: SHandle (Position, Velocity) Position -> System Asteroids ()
    myGame h = do
        liftIO (threadDelay 10000)
        cmapSF h 0.01 position
    position :: (Position, Velocity) -> SF a Position 
    position (Position x0, Velocity v0) =
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
