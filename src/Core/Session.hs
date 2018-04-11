module Core.Session(
                    getInput
                   ,writeOutput
                   ,compute
                   )
                   where


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
import Core.Depression(balanceContribs
                      ,totalDecidFund
                      ,totalAvailFund)
import Core.Prosperity(reduceContribs
                      ,totalDecidedFundByBill)
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


-- read data from input, this function is not implemented with concrete Monad so it can be tested without the
-- need for IO rahter using pure code only
-- also this approach is used to follow the Hexagonal or port and adapter architecture
getInput :: (I.JSONRepo m ) => ExceptT T.JSONError m T.JData
getInput  = do
  result <- lift $ I.readInput 
  jsonData  <- case result of
    Nothing -> throwError  T.JSONErrorReadingFile
    (Just bStr) -> lift'' $ eitherDecode bStr 
  return jsonData

-- 
writeOutput :: (ToJSON a,I.JSONRepo m) => a -> ExceptT T.JSONError m ()
writeOutput   =lift . I.writeOutput  . encodePretty 

-- where the computation on the input happens
compute :: T.JData -> T.JData
compute val = T.JData (calcBillRcvFund output <$> bills  ) (calcRemFund <$> output)
 where
   districts = val ^. T.districts
   bills = val ^. T.bills
   priority1 = normalize districts bills
   priority2 =  capping priority1 bills
   priority3 = balanceContribs priority2 bills
   output = reduceContribs priority3 bills 

-- caclulate the remaining amount from the collected tax
-- this amount is to be refunded to to the people of the appropriate district
calcRemFund :: T.District -> T.District
calcRemFund  dist = 
  dist { T._distRefundAmount = Just $ (totalAvailFund dist) - (totalDecidFund dist) } 

-- calculate the actual received amount after all disrticts have decided and the three
-- law of proportionality has been applied
calcBillRcvFund :: [T.District] ->T.Bill  -> T.Bill
calcBillRcvFund  districts bill =
  bill { T._billReceivedFund  = Just billRcvFund }
  where
    billId = bill ^. T.billName
    billRcvFund = totalDecidedFundByBill billId districts
    billNeededFund = bill ^. T.billAmount

lift'' ::(Monad m) => Either String a -> ExceptT T.JSONError m a
lift'' val  = case val of
  Left err -> throwError $ T.JSONErrorWrongFormat err
  Right val -> return val


