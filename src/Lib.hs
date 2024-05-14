{-# LANGUAGE FlexibleInstances     #-}
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
    ( someFunc
    ) where


import Apecs
import FRP.Yampa
import Linear.V2
import Control.Concurrent
import Control.Monad
import Debug.Trace
import GHC.Generics
import Data.TreeDiff.Class
import Data.TreeDiff.Pretty
import Data.IORef
import Control.Concurrent.MVar
import Linear    as L

instance (Eq a, Floating a) => VectorSpace (V2 a) a where
  zeroVector = L.zero
  (*^) = (L.*^)
  --(^) = (L.^)
  negateVector = L.negated
  (^+^) = (L.^+^)
  (^-^) = (L.^-^)
  dot = L.dot


data Foo = Foo Int String deriving (Generic,ToExpr)



data Input  -- all types of events
data Output -- all those backend models
runGame ::
    SF Input (System w Output) ->
    --System w (SF Input Output) ->
    IO ()
runGame game = do
    undefined


newtype Position = Position (V2 Double) deriving Show
newtype Velocity = Velocity (V2 Double) deriving Show
data Flying = Flying
type SecretType = SF Input Position
makeWorldAndComponents "Asteroids" [''Position, ''Velocity, ''Flying]


runPositionSystem = do
    ref <- liftIO $ newIORef (Position 0)
    let positionSF = \(Position x0, Velocity v0) -> 
                            proc input -> do 
                                (x1 :: V2 Double) <- (arr (+x0) <<< integral) -< v0
                                (returnA :: SF a a) -< Position x1
    handle <- liftIO $ reactInit (pure NoEvent) (\handle updated pos -> when updated (writeIORef ref pos) >> pure False) (proc ev -> do drSwitch (positionSF (Position 0, Velocity 0)) -< ((), ev))
    forever do
        liftIO (threadDelay 100000)
        cmapSF ref handle 0.1 positionSF
        cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p)

cmapSF ref handle dt fsf = do
    cmapM 
        $ \components -> do
            liftIO do
                _ <- react handle (dt, Just (Event (fsf components)))
                readIORef ref


someFunc :: IO ()
someFunc = do
    asteroids <- initAsteroids
    runSystem app asteroids
  where
    app = do
        newEntity (Position 0, Velocity 1)
        newEntity (Position 2, Velocity 1)
        newEntity (Position 1, Velocity 2)
        runPositionSystem
    --print (prettyEditExpr $ ediff (toExpr $ Foo 2 "opa") (toExpr $ Foo 0 "opa"))
