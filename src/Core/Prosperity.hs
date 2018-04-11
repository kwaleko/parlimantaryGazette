module Core.Prosperity  where

import           Core.PlannedFund(lookupDecidedFund,(/))
import qualified Core.Types   as T

import           Prelude hiding ((/))
import           Control.Lens(over
                             ,traverse
                             ,(%~)
                             ,(^.)
                             ,(&)
                             ,(.~))


--                                          U S E  C A S E 
--        IN CASE THE TOTAL FUNDING DECIDED ACCROSS ALL DISTRICY EXEEDS THE AMOUNTOF  A
--        SPECIFIC BILL THE CONTRIBUTION BY EACH DISTRICT SHOULD BE REDUCE PROPORTIONALLY 


import           Prelude hiding ((/))
-- Reduce the contribution for all bills across all districts when needed
reduceContribs :: [T.District] -> [T.Bill] ->[T.District]
reduceContribs dists bills = 
  dists >>= \dist -> return $ proportion dist dists bills


proportion ::  T.District -> [T.District] -> [T.Bill] -> T.District
proportion  currDist dists bills  = let
  totalFunds  = \billId ->   totalDecidedFundByBill billId dists
  neededFund  = \bill ->  bill ^. T.billAmount
  actualFund = \bill -> bill ^. T.billSpecificFundingAmount 
  in over (T.distBillSpecificFund . traverse) (transform actualFund neededFund totalFunds) currDist
  where
    transform  :: (T.BillSpecificFunding -> Int)
               -> (T.Bill -> Int)
               -> (T.BillId -> Int)
               -> T.BillSpecificFunding
               -> T.BillSpecificFunding
    transform actualFund neededFund totalFunds specificFunding =
      let
        totalFundAmount = totalFunds $ specificFunding ^. T.billSpecificFundingBill
        neededFundAmount =  neededFund $ lookupBillById ( specificFunding ^. T.billSpecificFundingBill)  bills
        actualFundAmount = actualFund  specificFunding
      in
        if (totalFundAmount > neededFundAmount )
        then specificFunding & T.billSpecificFundingAmount %~ (\x -> ( (x *  neededFundAmount) / totalFundAmount))
        else specificFunding

--  warning : this is not safe function, I should come up with better design to avoid such unsafe option
lookupBillById :: T.BillId -> [T.Bill] -> T.Bill
lookupBillById billId bills = head $ filter predicate  bills
  where
    predicate :: T.Bill -> Bool
    predicate bill = bill ^. T.billName == billId

--  Total funding decided across all district for a specific bill
totalDecidedFundByBill :: T.BillId -> [T.District] -> Int
totalDecidedFundByBill billId districts =
  foldl (+) 0 $  accumulate billId <$>  districts
  where
    accumulate :: T.BillId -> T.District -> Int
    accumulate billId district  =
      lookupDecidedFund billId  $ district ^. T.distBillSpecificFund


