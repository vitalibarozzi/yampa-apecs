{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Apecs
import Apecs.Yampa
import Control.Concurrent
import Control.Monad
import Data.Text as Text
import FRP.Yampa
import Linear as L
import Linear.V2

-----------------------------------------------------------
newtype Position = Position (V2 Double) deriving (Show)

-----------------------------------------------------------
newtype Velocity = Velocity (V2 Double) deriving (Show)

-----------------------------------------------------------
type PosHandle = ApecsHandle (Position, Velocity) Position

-----------------------------------------------------------
makeWorldAndComponents
    "World"
    [ ''Position
    , ''Velocity
    , ''PosHandle
    ]

-----------------------------------------------------------
main :: IO ()
main = do
    world <- initWorld
    runSystem myApp world
  where
    myApp :: System World ()
    myApp = do
        let foo = cmapM \(Position p) -> liftIO (putStrLn $ show p <> "\n")

        reactiveEntt do
            entityHdl <- reactiveComp positionSF (Position 0)
            return (Velocity 1, entityHdl)

        cmapSF 10 positionSF >> foo
        cmapSF 10.1 positionSF >> foo
        cmapSF 0 positionSF >> foo
        cmapSF (-13) positionSF >> foo
        cmapSF 13 positionSF >> foo

    positionSF :: SF (Position, Velocity) Position
    positionSF =
        proc (Position x0, Velocity v0) -> do
            dx <- integral -< v0
            returnA -< Position (x0 + dx)

-----------------------------------------------------------
instance (Eq a, Floating a) => VectorSpace (V2 a) a where
    zeroVector = L.zero
    (*^) = (L.*^)

    -- (^) = (L.^)
    negateVector = L.negated
    (^+^) = (L.^+^)
    (^-^) = (L.^-^)
    dot = L.dot
