module Core.Depression where

import qualified Core.Types as T
import  Core.PlannedFund((/),lookupDecidedFund)
import Control.Lens( over
                    ,(^.)
                    )
import Prelude hiding((/))


--                                                        U S E  C A S E
--                 IN CASE THE TOTAL FUND COLLECT BY A GIVEN DISTRICT IS LESS THAN THE SUM OF AMOUNT THAT
--                 THE DISTRICT WHISHES TO PAY TO FUND VARIOUS FILL, THEN THE DECIDED FUND BY THE GIVEN
--                 DISTRICT SHOULD BE UPDATED AS PER THE PRINCIPLE OF PROPOSTIONALITY

balanceContribs ::      [T.District]
                    ->  [T.Bill]
                    ->  [T.District]
balanceContribs  dists bills =
  dists >>= \dist -> balanceContrib dist bills
  where
    balanceContrib :: T.District -> [T.Bill] -> [T.District]
    balanceContrib dist bills = if totalAvailFund dist < totalDecidFund dist
      then return $ proportion dist bills
      else return dist

proportion :: T.District -> [T.Bill] -> T.District
proportion dist bills = let
  billFund  = \bill -> lookupDecidedFund bill $ dist ^. T.distBillSpecificFund
  collectedTax  = totalAvailFund dist
  totalDecidedFund = totalDecidFund dist
  in over (T.distBillSpecificFund . traverse ) (transform billFund collectedTax totalDecidedFund) dist
  where
    transform  :: (T.BillId -> Int) -> Int -> Int -> T.BillSpecificFunding -> T.BillSpecificFunding
    transform  billFund collectedTax  totalDecidedFund billSpecFund =
      T.BillSpecificFunding 
       (billSpecFund ^. T.billSpecificFundingBill) $
       (billFund (billSpecFund ^. T.billSpecificFundingBill) * collectedTax) / totalDecidedFund


-- Total Tax collected by a give district
totalAvailFund :: T.District -> Int
totalAvailFund  dist = dist ^. T.distAvailableFund

-- Sum of total amount that a district wishes to pay or plan to pay 
totalDecidFund :: T.District -> Int
totalDecidFund dist  =
  foldl (+) 0 $ T._billSpecificFundingAmount <$> dist ^.  T.distBillSpecificFund
