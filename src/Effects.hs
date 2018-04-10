{-#Language FlexibleInstances#-}
{-#Language GeneralizedNewtypeDeriving#-}
module Effects where

import Control.Monad.Reader 

import qualified Core.Interfaces as I
import qualified Adapter.JSON    as JSON
import qualified Core.Types      as T

newtype AppM a = AppM (ReaderT FilePath IO a)
  deriving(Functor,Applicative,Monad,MonadIO,MonadReader FilePath)

runAppM :: FilePath -> AppM a -> IO a
runAppM path (AppM act) = runReaderT act path

instance I.JSONRepo AppM where
  readInput = JSON.readJSON
  writeOutput = JSON.writeJSON

