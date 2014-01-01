{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Control.Eff.Log
  ( Log(..)
  , logTo
  , runLog
  , runLogStdErr
  , runLogM
  ) where

import Control.Eff ( Eff, Member, SetMember, VE(..), (:>)
                   , admin, handleRelay, inj, send)
import Control.Eff.Lift (Lift, lift)
import Data.Typeable (Typeable, Typeable1)

data Log a b v = Log a b v
  deriving (Typeable, Functor)

logNext :: Log a b v -> v
logNext (Log _ _ v) = v

logTo :: (Typeable f, Typeable c, Member (Log f c) r)
  => f -> c -> Eff r ()
logTo facility line = send $ \next -> inj (Log facility line (next ()))

runLog :: (Typeable l, Typeable t)
  => (forall v. Log l t v -> String)
  -> Eff (Log l t :> r) a
  -> Eff r (a, [String])
runLog formatter = go . admin
  where go (Val v) = return (v, [])
        go (E req) = handleRelay req go performLog
        performLog l = fmap (prefixLogWith (formatter l)) (go (logNext l))
        prefixLogWith txt (v, l) = (v, txt : l)

runLogStdErr :: (Typeable l, Typeable t, SetMember Lift (Lift IO) r)
  => (forall v. Log l t v -> String) -> Eff (Log l t :> r) a -> Eff r a
runLogStdErr formatter = runLogM (putStrLn . formatter)

runLogM :: (Typeable l, Typeable t, Typeable1 m, SetMember Lift (Lift m) r)
  => (forall v. Log l t v -> m ()) -> Eff (Log l t :> r) a -> Eff r a
runLogM logger = go . admin
  where go (Val v) = return v
        go (E req) = handleRelay req go performLog
        performLog l = lift (logger l) >> go (logNext l)
