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


type Foo = ApecsHandle (Position,Velocity) Position

-----------------------------------------------------------
makeWorldAndComponents "World" [''Position, ''Velocity, ''Foo]


-----------------------------------------------------------
main :: IO ()
main = do
    world <- initWorld
    runSystem myApp world
  where
    myApp :: System World ()
    myApp = do

        let foo = cmapM \(Position p) -> liftIO (putStrLn $ show p <> "\n")
        
        entityHdl <- reactInitSF positionSF (Position 0, Velocity 0)
        newEntity_ (Position 0, Velocity 1, entityHdl)

        cmapSF 10    positionSF >> foo
        cmapSF 10.1  positionSF >> foo
        cmapSF 0     positionSF >> foo
        cmapSF (-13) positionSF >> foo
        cmapSF 13    positionSF >> foo

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
