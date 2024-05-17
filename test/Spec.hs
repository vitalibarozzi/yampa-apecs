{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE Arrows     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Lib
import Apecs
import FRP.Yampa
import Data.Text as Text
import Linear.V2
import Linear    as L
import FRP.Yampa.Brick
import Brick
import Brick.Widgets.Core
import Control.Concurrent
import Control.Monad


-----------------------------------------------------------
newtype Position = Position (V2 Double) deriving Show


-----------------------------------------------------------
newtype Velocity = Velocity (V2 Double) deriving Show


-----------------------------------------------------------
makeWorldAndComponents "World" [''Position, ''Velocity]


-----------------------------------------------------------
main :: IO ()
main = do
    asteroids <- initWorld
    runSystem myApp asteroids
  where
    myApp = do

        posHdl <- newCHandle
        fooHdl <- newCHandle
        let brickSF = proc (a :: [(Position,Entity)]) -> do t <- time -< (); returnA -< txt (Text.pack (show t) <> ":" <> Text.pack (show a))
        callbackHandle <- liftIO $ reactInit (pure NoEvent) (\_ _ xs -> pure False) returnA
        brickHdl <- liftIO $ reactInitBrick [] brickSF callbackHandle

        _ <- newEntity (Position 0 , Velocity 1)
        _ <- newEntity (Position 50, Velocity (-1))
        cmapHandle posHdl (10, positionSF)
        _ <- newEntity (Position 100, Velocity (-1))
        cmapHandle posHdl (10, positionSF)
        --cmapM_ \(Position p, Entity e) -> liftIO . print $ (e, p)
        _ <- newEntity (Position 200, Velocity (-10))

        forever do
            liftIO (threadDelay 100000)
            cmapHandle posHdl (0.1, positionSF)
            xs <- cfoldHandle fooHdl (0.1, \acc (Position p, Entity e) -> pure ((Position p, Entity e) : acc)) []
            liftIO $ react brickHdl (0.1, Just xs) 
       
        pure ()

    positionSF :: (Position, Velocity) -> SF a Position 
    positionSF (Position x0, Velocity v0) =
        proc ____input -> do 
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
