module Core.Interfaces where

import Data.Aeson
import Data.Text.Lazy
import Data.ByteString.Lazy 

class (Monad m) => JSONRepo m where
  readInput      ::  m (Maybe ByteString)
  writeOutput    ::  ByteString -> m ()
