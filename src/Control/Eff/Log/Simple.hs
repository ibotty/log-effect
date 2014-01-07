{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Eff.Log.Simple
  ( SimpleLog
  , Severity(..)
  , logTo
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
import Control.Eff (Eff, SetMember)
import Control.Eff.Log
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import System.Log.FastLogger (toLogStr)

data Severity = DEBUG | INFO | NOTICE | WARNING | ERROR | CRITICAL | ALERT | PANIC
  deriving (Bounded, Enum, Eq, Ord, Read, Show, Typeable)

logTo :: (Typeable l, SetMember Log (Log (Severity, l)) r)
  => Severity -> l -> Eff r ()
logTo sev line = logE (sev, line)

debug :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
debug = logTo DEBUG

info :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
info = logTo INFO

notice :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
notice = logTo NOTICE

warning :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
warning = logTo WARNING

error :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
error = logTo ERROR

critical :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
critical = logTo CRITICAL

alert :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
alert = logTo ALERT

panic :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
panic = logTo PANIC

type SimpleLog a = Log (Severity, a)

instance ShowLog a => ShowLog (Severity, a) where
    showLog (sev, line) = "[" <> toLogStr (show sev) <> "] " <> showLog line
