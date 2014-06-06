{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Eff
import Control.Eff.Lift
import Control.Eff.Log
import System.Log.FastLogger

data Proxy a = Proxy

main :: IO ()
main = do
    loggerSet <- newStderrLoggerSet defaultBufSize
    pushLogStr loggerSet "logging from outer space^WIO\n"
    runLift $ runLogWithLoggerSet loggerSet (Proxy :: Proxy String) $
        logE ("logging from inner Eff" :: String)
    flushLogStr loggerSet
