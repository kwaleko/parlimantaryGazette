module Core.Session where


import Control.Monad.Except
import Data.Aeson(encode
                 ,decode
                 ,eitherDecode
                 ,ToJSON
                 ,FromJSON)
import Data.Aeson.Text(encodeToLazyText)
import Data.Aeson.Encode.Pretty(encodePretty)
import Data.List(find)
import Data.Maybe
import Data.ByteString.Lazy
import Core.Parsing
import Core.Depression(balanceContribs)
import Core.Prosperity(proportion,reduceContribs)
import Core.PlannedFund(normalize)
import Core.Capping(capping)
import Control.Lens((^.))


import qualified  Core.Types      as T
import qualified  Core.Interfaces as I

getInput :: (I.JSONRepo m ) => ExceptT T.JSONError m T.JData
getInput  = do
  result <- lift $ I.readInput 
  jsonData  <- case result of
    Nothing -> throwError  T.JSONErrorReadingFile
    (Just bStr) -> lift'' $ eitherDecode bStr 
  return jsonData

compute :: T.JData -> [T.District]
-- compute val =   balanceContribs  (normalize districts    bills)    bills
--compute val =   reduceContribs bills (normalize districts    bills)
-- compute val =   capping  (normalize districts    bills)  bills
compute val = reduceContribs bills  (normalize districts    bills)  
 where
   districts = val ^. T.districts
   bills = val ^. T.bills
   
lift'' ::(Monad m) => Either String a -> ExceptT T.JSONError m a
lift'' val  = case val of
  Left err -> throwError $ T.JSONErrorWrongFormat err
  Right val -> return val

 
writeOutput :: (ToJSON a,I.JSONRepo m) => a -> ExceptT T.JSONError m ()
writeOutput   =lift . I.writeOutput  . encodePretty 



