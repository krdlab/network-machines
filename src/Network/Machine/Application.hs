{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Network.Machine.Application (
      Context(..)
    , Application
    )where

import Data.Machine     (SourceT, ProcessT)
import Data.ByteString  (ByteString)

data Context m = Context
    { source :: SourceT  m ByteString
    , sink   :: ProcessT m ByteString ()
    }

type Application m = Context m -> m ()

