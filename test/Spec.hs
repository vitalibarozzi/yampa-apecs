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

import Apecs.Yampa
import Apecs
import FRP.Yampa
import Data.Text as Text
import Linear.V2
import Linear    as L
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
    world <- initWorld
    runSystem myApp world
  where
    myApp :: System World ()
    myApp = do
        newEntity_ (Position 0,Velocity 1)
        newEntity_ (Position 40,Velocity 1)
        newEntity_ (Position 100,Velocity 1)
        cmapSF positionSF 10
        cmapM \(Position p) -> liftIO (print p)
        pure ()

    positionSF :: SF (Position, Velocity) Position 
    positionSF =
        proc (Position x0, Velocity v0) -> do 
            dx <- integral -< v0
            returnA -< Position (x0 + dx)


-----------------------------------------------------------
instance (Eq a, Floating a) => VectorSpace (V2 a) a where
  zeroVector = L.zero
  (*^) = (L.*^)
  --(^) = (L.^)
  negateVector = L.negated
  (^+^) = (L.^+^)
  (^-^) = (L.^-^)
  dot = L.dot
