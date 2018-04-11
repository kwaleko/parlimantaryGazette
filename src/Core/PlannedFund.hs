module Core.PlannedFund 
where

import Data.List(find)
import Prelude hiding ((/))
import Data.Maybe(fromJust,isJust)

import qualified  Core.Types as T

import Control.Lens ((^.))


-- the purpose from this function is that each bill should be present in the specific fund list
-- either it was provided by the original file
-- or deduced form the category
normalize :: [T.District]
          -> [T.Bill]
          -> [T.District]
normalize districts bills =
  map (\district -> district {T._distBillSpecificFund = districtSpecFund district bills } ) districts

-- Deduce All bills Planned Fund by a given district
-- in case the District has Specified a fund for a bill, it is prioritize
-- otherwise, the fund amount will be equal to the default fund of the given category
districtSpecFund :: T.District
                 -> [T.Bill]
                 -> [T.BillSpecificFunding]
districtSpecFund district bills  = do
  bills >>= \bill -> return $
    deduceAmount bills categoryFund districtSpecificFunding $  bill ^. T.billName
  where
    categoryFund = district ^. T.distCategDefault
    districtSpecificFunding = district ^. T.distBillSpecificFund

    -- deduceAmount
  -- <$> bills
 -- <*> pure (district ^. T.distCategDefault )
 -- <*> pure (district ^. T.distBillSpecificFund) 
deduceAmount :: [T.Bill]
             -> [T.CategoryDefaultFunding]
             -> [T.BillSpecificFunding]
             -> T.BillId
             -> T.BillSpecificFunding
deduceAmount  _ _ specFund billId | isFundSpecified billId specFund =
                               T.BillSpecificFunding billId $ fromJust $ lookupDecidedFundPerBill billId specFund
deduceAmount  bills  categLst _ billId | isDefaultSpecified billId bills categLst =
                                 T.BillSpecificFunding billId $ fromJust $ lookupDefaultFund billId bills categLst
deduceAmount   _ _  _ billId                                       =
   T.BillSpecificFunding billId  0

isFundSpecified :: T.BillId
                -> [T.BillSpecificFunding]
                -> Bool
isFundSpecified billId billSpecFund =
  isJust $ lookupDecidedFundPerBill billId billSpecFund

isDefaultSpecified :: T.BillId
                   -> [T.Bill]
                   -> [T.CategoryDefaultFunding]
                   -> Bool
isDefaultSpecified billId bills defaultFund =
  isJust $ lookupDefaultFund billId bills defaultFund 


lookupDefaultFund :: T.BillId
                  -> [T.Bill]
                  -> [T.CategoryDefaultFunding]
                  -> Maybe Int
lookupDefaultFund  billId bills categLst = do
  category <- lookupCategoryByBill billId bills
  lookup category $
    zipWith (,)  (map T._categoryDefaultFundingCategory categLst) (map T._categoryDefaultFundingAmount categLst)

lookupDecidedFundPerBill :: T.BillId
                         -> [T.BillSpecificFunding]
                         -> Maybe Int
lookupDecidedFundPerBill billId specLst =
  lookup  billId $
  zipWith (,) (map T._billSpecificFundingBill specLst ) (map T._billSpecificFundingAmount specLst)

-- bad design choice to use fromJust,
-- The fact that each bill should be in the specific fund list should be expressed in the type for the Parser output(structure)
-- after the above is proved, it would be safe to write a lookup that does not fail
-- for now, I will assume it is proved, I don't have enough time to make it work
lookupDecidedFund ::  T.BillId
                   -> [T.BillSpecificFunding]
                   ->  Int
lookupDecidedFund billId lst = fromJust $ lookupDecidedFundPerBill billId lst


lookupCategoryByBill :: T.BillId -> [T.Bill] -> Maybe T.Category
lookupCategoryByBill billId bills =
  lookup  billId $
  zipWith (,) (map T._billName bills ) (map T._billCategory bills)

-- custom div to cater for the rounding cases
(/) :: Int -> Int -> Int
(/) nb divisor
  | nb `rem` divisor == 0        =  nb `div` divisor
  | (2 * (nb `rem` divisor)) >= divisor  =(+) 1 $ nb `div` divisor
  | True                         =   nb `div` divisor

