{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Machine (runT_, (~>), auto)
import qualified Data.Text as T (toUpper)
import qualified Data.Text.Encoding as T (decodeUtf8, encodeUtf8)
import qualified Network.Machine as NM
import qualified Network.Machine.Application as App

main :: IO ()
main = NM.runServer (NM.settings 3000) app
    where
        app ctx = echo (App.source ctx) (App.sink ctx)
        echo src dst = runT_ $
            src
                ~> auto T.decodeUtf8 -- XXX: 途中で切れると正しくデコードされない，Data.Conduit.Text のようなものが必要
                ~> auto T.toUpper
                ~> auto T.encodeUtf8
                ~> dst

