{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Apecs.Yampa (
    cmapSF,
    ApecsHandle,
    reactiveEntt,
    reactiveComp,
    -- makeWorldAndComponents_,
)
where

import Apecs
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import FRP.Yampa hiding (reactInit)
import qualified FRP.Yampa as Yampa
import Language.Haskell.TH

-----------------------------------------------------------

{- | Convenience function so we can pass the types directly
makeWorldAndComponents_ :: String -> [String] -> Q [Dec]
makeWorldAndComponents_ world namesStr = do
   let names = fmap undefined namesStr
   makeWorldAndComponents world names
-}

-----------------------------------------------------------

-- | Convenience function.
reactiveEntt ::
    ( MonadIO m
    , Set w m cx
    , Has w m EntityCounter
    ) =>
    SystemT w m cx ->
    SystemT w m ()
{-# INLINE reactiveEntt #-}
reactiveEntt initStuff = do
    sfs <- initStuff
    newEntity_ sfs

-----------------------------------------------------------

reactiveComp ::
    forall cx cy w m.
    (MonadIO m) =>
    SF cx cy ->
    cy ->
    SystemT w m (cy, ApecsHandle cx cy)
{-# INLINE reactiveComp #-}
reactiveComp !sf !initialValue = do
    liftIO do
        !ref <- newIORef (error "never called")
        !hdl <- Yampa.reactInit setup (actuate ref) (wrap (drSwitch sf))
        threadDelay 1
        return (initialValue, ApecsHandle (ref, hdl))
  where
    setup = pure (Nothing, NoEvent)

    actuate !ref _ !updated cy = do
        when updated (writeIORef ref cy)
        pure updated

    wrap :: SF (cx, Event (SF cx cy)) cy -> SF (Maybe cx, Event (SF cx cy)) cy
    wrap !ss = proc (!mx, !ev) -> do
        case mx of
            Just !x -> ss -< (x, ev)
            Nothing -> returnA -< initialValue

-----------------------------------------------------------

{- | Use an SF to map all entities that have the components
cx and cy associated with a handler.
-}
cmapSF ::
    forall w m cx cy.
    ( Members w m cx
    , Get w m cx
    , Members w m cy
    , Get w m cy
    , Set w m cy
    , Members w m (ApecsHandle cx cy)
    , Get w m (ApecsHandle cx cy)
    , MonadIO m
    ) =>
    DTime ->
    SF cx cy ->
    SystemT w m ()
{-# INLINEABLE cmapSF #-}
cmapSF dt sf = do
    cmapM
        \( ApecsHandle (ref, hdl) :: ApecsHandle cx cy
            , cx :: cx
            , cy :: cy
            ) ->
                liftIO do
                    updated1 <- react hdl (0, Just (Just cx, Event sf))
                    if updated1
                        then do
                            !updated2 <- react hdl (dt, Nothing)
                            if updated2
                                then readIORef ref
                                else pure cy
                        else pure cy

-----------------------------------------------------------

{- | A Yampa handle wrapped. Double-duty to speed up Apecs
as it makes the component more specific when we use it to
create a new entity. See Apecs.
-}
newtype ApecsHandle cx cy
    = ApecsHandle
        ( IORef cy
        , ReactHandle (Maybe cx, Event (SF cx cy)) cy
        )
