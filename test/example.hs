{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Machine (runT_, (~>), auto)
import qualified Data.Text as T (toUpper)
import qualified Data.Text.Encoding as T (decodeUtf8, encodeUtf8)
import qualified Network.Machines as NM
import qualified Network.Machines.Application as App

main :: IO ()
main = NM.runServer (NM.settings 3000) app
    where
        app ctx = echo (App.source ctx) (App.sink ctx)
        echo src dst = runT_ $
            src
                ~> auto T.decodeUtf8
                ~> auto T.toUpper
                ~> auto T.encodeUtf8
                ~> dst

