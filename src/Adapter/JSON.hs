{-# LANGUAGE FlexibleContexts #-}
module Adapter.JSON where

import qualified Core.Types as T

import           Control.Monad.Reader
import qualified Data.ByteString.Lazy as BS
import Data.Text.Lazy
import Data.Text.Lazy.IO as TL


-- Validation wether the file path exist should be added..
readJSON :: (MonadIO m,MonadReader FilePath m) => m (Maybe BS.ByteString)
readJSON = do
  fPath <- ask
  liftIO $ Just <$> BS.readFile fPath

writeJSON :: (MonadIO m,MonadReader FilePath m) => BS.ByteString -> m ()
writeJSON output = do
  fPath <- ask
  liftIO $ BS.writeFile fPath output
