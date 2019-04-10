module Prelude (module Relude, io) where

import Relude

io :: MonadIO m => IO a -> m a
io = liftIO