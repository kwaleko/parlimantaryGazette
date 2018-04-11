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
import Core.Prosperity(reduceContribs)
import Core.PlannedFund(normalize)
import Core.Capping(capping)
import Control.Lens((^.))

                                   

import qualified  Core.Types      as T
import qualified  Core.Interfaces as I


--                                    P R I O R I T Y
-- FIRST OF ALL, THE DATA IS TO BE NORMALIZED SO ALL BILLS FUND ARE AVAILABLE IN THE SPECIFIC FUND FOR ALL DISTRICTS
-- THE DECIDED PRIORITY IS TO CHECK THE MAX CAP FIRST
-- THEN TO CHECK WHETHER A DISTRICT CAN AFFORD ALL DECIDED BILL (DEPRESSION USE CASE)
-- THEN TO CHECK WETHER A BILL HAVE ADDITIONAL FUND AND REDUCE IT AS PER THE PROPORTIONALLY (PROSPERITY USE CASE)
-- AS PER THE ABOVE APPROACH, THIS MAKE PRIORITY FOR A BILL TO FULLY FUNDED THAN WELL DISTRIBUTE FUND ACROSS A DISTRICT



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
compute val =  priority4
 where
   districts = val ^. T.districts 
   bills = val ^. T.bills
   priority1 = normalize districts bills
   priority2 =  capping priority1 bills
   priority3 = balanceContribs priority2 bills
   priority4 = reduceContribs priority3 bills 
   
lift'' ::(Monad m) => Either String a -> ExceptT T.JSONError m a
lift'' val  = case val of
  Left err -> throwError $ T.JSONErrorWrongFormat err
  Right val -> return val

 
writeOutput :: (ToJSON a,I.JSONRepo m) => a -> ExceptT T.JSONError m ()
writeOutput   =lift . I.writeOutput  . encodePretty 



