{-# LANGUAGE FlexibleInstances     #-}
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


data Foo = Foo Int String deriving (Generic,ToExpr)


someFunc :: IO ()
someFunc = do
    print (prettyEditExpr $ ediff (toExpr $ Foo 2 "opa") (toExpr $ Foo 0 "opa"))


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
makeWorldAndComponents "Asteroids" [''SecretType, ''Position, ''Velocity, ''Flying]


runSys = undefined


sfGame :: SF Input (System Asteroids Output) 
sfGame = parB myGame


myGame :: System Asteroids (SF Input Output) 
myGame = do
    --cmapMsf_ $ \(Position x0, Velocity v0) -> proc input -> do 
    --     x1 <- (arr (+x0) <<< integral) -< v0
    --     returnA -< Position x1
    --cmapMreact $ \(Position p, Entity e) -> liftIO . print $ (e, p)
    undefined



somePosition :: Float -> Float -> SF () Float
somePosition x0 v0 = proc () -> do
    (arr (+x0) <<< integral) -< v0


javai :: System Asteroids (SF () Float) 
javai = do
    pure $ proc () -> do
        returnA -< 0

{-
    print "----"
    h <- reactInit (return False) (\h u a -> threadDelay 1000000 >> print "actuate" >> pure a) returnA
    print "..."
    p <- react h (0, Just True)
    print p
    p <- react h (0, Just False)
    print p
    print "done"
    h <- reactInit (print "help" >> pure False) (\handle updated a -> when (not updated) undefined >> threadDelay 1000 >> print a >> return a) returnA
    print =<< react h (0.00, Nothing)
    print =<< react h (0.00, Nothing)
    print =<< react h (0.00, Nothing)
    print =<< react h (-1.00, Just True)
    print =<< react h (0.00, Nothing)
    print =<< react h (-1.00, Just True)
    print =<< react h (-1.00, Nothing)
    print =<< react h (0.00, Nothing)
    print =<< react h (-1.00, Nothing)
    print =<< react h (0.00, Nothing)
    print =<< react h (0.00, Just False)
    print =<< react h (1.00, Nothing)
    print =<< react h (0.00, Nothing)
    print =<< react h (-1.00, Nothing)
    print =<< react h (0.00, Nothing)
    print =<< react h (-1.00, Just True)
    print =<< react h (0.00, Just False)
    print =<< react h (0.00, Nothing)
    print =<< react h (1.00, Nothing)
    print =<< react h (0.00, Nothing)
    threadDelay 5000000
    print =<< react h (0.00, Nothing)
    print =<< react h (0.00, Nothing)
    print =<< react h (0.00, Nothing)
    print =<< react h (-1.00, Just True)
    print =<< react h (0.00, Nothing)
    print =<< react h (-1.00, Just True)
    print =<< react h (-1.00, Nothing)
    print =<< react h (0.00, Nothing)
    print =<< react h (-1.00, Nothing)
    print =<< react h (0.00, Nothing)
    print =<< react h (0.00, Just False)
    print =<< react h (1.00, Nothing)
    print =<< react h (0.00, Nothing)
    print =<< react h (-1.00, Nothing)
    print =<< react h (0.00, Nothing)
    print =<< react h (-1.00, Just True)
    print =<< react h (0.00, Just False)
    print =<< react h (0.00, Nothing)
    print =<< react h (1.00, Nothing)
    print =<< react h (0.00, Nothing)
    threadDelay 5000000
    react h (0.00, Nothing)
    pure ()


bar :: IO ()
bar = do
    h <- foo "hi" (arr putStrLn)
    react h (0.00, Nothing)
    react h (0.01, Just "hey")
    forkIO $ forever $ react h (0.01, Nothing)
    forever $ do
        threadDelay 10000
        l <- getLine
        react h (0.00, Just l)
    pure ()


foo :: a -> SF a (IO ()) -> IO (ReactHandle a (IO ()))
foo a sf = do
    reactInit
        (pure a)
        (\handle updated io -> when (not updated) undefined >> when updated io >> pure False)
        sf


{-
newtype Position = Position (V2 Double) deriving Show
newtype Velocity = Velocity (V2 Double) deriving Show
data Flying = Flying


makeWorldAndComponents "World" [''Position, ''Velocity, ''Flying]


data Asteroid f = Asteroid
    { siz :: f Float
    , vel :: f Velocity
    , pos :: f Position
    }


position :: V2 Double -> SF () Position
position = undefined


velocity :: V2 Double -> SF () Double
velocity = undefined


flying :: SF () Flying
flying = undefined


game :: System World ()
game = do

  newEntity (position 0, velocity 1)
  newEntity (position 2, velocity 1)
  newEntity (position 1, Velocity 2, flying)

  -- handleBrick  <- reactInitBrick  ...
  -- handleZ3     <- reactInitZ3     ...
  -- handleOpenAL <- reactInitOpenAL ...
  -- handleOpenGL <- reactInitOpenGL ...
  -- handleOpenDE <- reactInitOpenDE ...

  -- 1. Add velocity to position
  -- 2. Apply gravity to non-flying entities
  -- 3. Print a list of entities and their positions
  cmap $ 
      \(position, velocity) -> proc a -> do
          x <- position -< a
          v <- velocity -< a

      
     position -< position-- Position (v+p)
  cmap $ \(Velocity v, _ :: Not Flying) -> Velocity (v - V2 0 1)
  cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p)


cmapSF :: SF (Position, Velocity) Position -> System World ()
cmapSF = undefined


someFunc :: IO ()
someFunc = do
    initWorld >>= runSystem game



positions :: SF a (System World [Position])
positions = proc a -> do
    returnA -< cfold \Position{..} acc -> Position{..} : acc



SF a (ECS b) ->
IO (ReactHandle a b)



reactInitIO ::
    a ->
    SF a (IO b) ->
    IO (ReactHandle a (IO b))
-}
-}
