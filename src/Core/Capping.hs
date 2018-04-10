module Core.Capping where

import qualified Core.Types as T

import  Core.PlannedFund((/),lookupCategoryByBill)
import Control.Lens(over,(^.))
import Prelude hiding((/))
import Data.Maybe(isJust,fromJust)



capping :: [T.District] ->  [T.Bill] ->  [T.District]
capping  dists bills =
  dists >>= \dist -> balanceContrib dist bills
  where
    balanceContrib :: T.District -> [T.Bill] -> [T.District]
    balanceContrib dist bills =
      return $ proportion dist bills


proportion ::  T.District -> [T.Bill] -> T.District
proportion  dist bills =
  let
    capAmount  = \categ -> maxCap dist categ
    totalDecidedFund  = \categ -> totalDecidedFundByCategory dist categ bills
  in
    over (T.distBillSpecificFund . traverse ) (transform  capAmount totalDecidedFund) dist
  where
    transform  :: (T.Category -> Int) -> (T.Category -> Int) -> T.BillSpecificFunding -> T.BillSpecificFunding
    transform    capAmount  totalDecidedFund  billSpecFund =
      let
        category = fromJust $ lookupCategoryByBill ( billSpecFund ^. T.billSpecificFundingBill) bills
        doBalance =  totalDecidedFund category > capAmount category 
      in
        case doBalance of
          True -> T.BillSpecificFunding 
            (billSpecFund ^. T.billSpecificFundingBill) $ 
            ((billSpecFund ^. T.billSpecificFundingAmount) * (capAmount category)) / (totalDecidedFund category)
          False -> billSpecFund


-- Total decided or planned fun for a given category in a given district
totalDecidedFundByCategory :: T.District
                           -> T.Category
                           -> [T.Bill]
                           -> Int
totalDecidedFundByCategory dist category bills =
  foldl (+) 0 $ map T._billSpecificFundingAmount $  filter (predicate category)  $ dist ^. T.distBillSpecificFund
  where 
    predicate :: T.Category -> T.BillSpecificFunding -> Bool
    predicate  category specFund =
     case isJust $ lookupCategoryByBill (specFund ^. T.billSpecificFundingBill) bills of
       True ->  fromJust ( lookupCategoryByBill (specFund ^. T.billSpecificFundingBill) bills ) == category
       False -> False


-- Cap for a given category
maxCap :: T.District
       -> T.Category
       -> Int
maxCap  dist category = fromJust $ lookup category $
  zipWith (,) (map T._capCategory categs) (map T._capAmount categs)
  where
    categs = dist ^. T.distCaps
