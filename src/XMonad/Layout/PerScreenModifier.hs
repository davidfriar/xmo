{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
  FlexibleContexts #-}

module XMonad.Layout.PerScreenModifier
  ( PerScreenModifier(..)
  , forScreen
  ) where

import Data.Maybe
import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.StackSet

data PerScreenModifier l1 l2 a =
  PerScreenModifier ScreenId (l1 a) (l2 a)
  deriving (Show, Read)

instance (Read (l1 a), Show (l1 a), Read (l2 a), Show (l2 a), LayoutClass l1 a, LayoutClass l2 a) =>
         LayoutModifier (PerScreenModifier l1 l2) a where
  modifyLayoutWithUpdate (PerScreenModifier s l1 l2) (Workspace wname _ st) r = do
    targetWS <- lookupWorkspace s <$> gets windowset
    if Just wname == targetWS
      then do
        (rs, ml1) <- runLayout (Workspace wname l1 st) r
        let l1' = fromMaybe l1 ml1
        l2' <- fromMaybe l2 <$> handleMessage l2 (SomeMessage Hide)
        return ((rs, Nothing), Just $ PerScreenModifier s l1' l2')
      else do
        (rs, ml2) <- runLayout (Workspace wname l2 st) r
        let l2' = fromMaybe l2 ml2
        l1' <- fromMaybe l1 <$> handleMessage l1 (SomeMessage Hide)
        return ((rs, Nothing), Just $ PerScreenModifier s l1' l2')
  handleMess (PerScreenModifier s l1 l2) mess = do
    l1' <- handleMessage l1 mess
    l2' <- handleMessage l2 mess
    if isNothing l1' && isNothing l2'
      then return Nothing
      else return $ Just $ PerScreenModifier s (fromMaybe l1 l1') (fromMaybe l2 l2')

forScreen :: ScreenId -> (l2 a -> l1 a) -> l2 a -> ModifiedLayout (PerScreenModifier l1 l2) l2 a
forScreen s f l = ModifiedLayout (PerScreenModifier s (f l) l) l
