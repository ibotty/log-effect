{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Control.Eff.Log.Simple
  ( SimpleLog
  , Severity(..)
  , runSimpleLog
  , runSimpleLogStdErr
  , showSimpleLog
  , debug
  , info
  , notice
  , warning
  , error
  , critical
  , alert
  , panic
  ) where

import Prelude hiding (error)
import Control.Eff (Eff, Member, SetMember, (:>))
import Control.Eff.Lift (Lift)
import Control.Eff.Log
import Data.Typeable (Typeable)

data Severity = DEBUG | INFO | NOTICE | WARNING | ERROR | CRITICAL | ALERT | PANIC
  deriving (Bounded, Enum, Eq, Ord, Read, Show, Typeable)

debug :: Member SimpleLog r => String -> Eff r ()
debug = logTo DEBUG

info :: Member SimpleLog r => String -> Eff r ()
info = logTo INFO

notice :: Member SimpleLog r => String -> Eff r ()
notice = logTo NOTICE

warning :: Member SimpleLog r => String -> Eff r ()
warning = logTo WARNING

error :: Member SimpleLog r => String -> Eff r ()
error = logTo ERROR

critical :: Member SimpleLog r => String -> Eff r ()
critical = logTo CRITICAL

alert :: Member SimpleLog r => String -> Eff r ()
alert = logTo ALERT

panic :: Member SimpleLog r => String -> Eff r ()
panic = logTo PANIC

type SimpleLog = Log Severity String

runSimpleLog :: Member SimpleLog r => Eff (SimpleLog :> r) a -> Eff r (a, [String])
runSimpleLog = runLog showSimpleLog

runSimpleLogStdErr :: (SetMember Lift (Lift IO) r, Member SimpleLog r) =>
    Eff (SimpleLog :> r) a -> Eff r a
runSimpleLogStdErr = runLogStdErr showSimpleLog

showSimpleLog :: SimpleLog v -> String
showSimpleLog (Log sev line _) = "[" ++ show sev ++ "] " ++ line
