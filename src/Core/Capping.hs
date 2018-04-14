module Core.Capping where

import qualified Core.Types as T

--import  Core.PlannedFund((/),lookupCategoryByBill)
import Control.Lens(over,(^.))
import Prelude
import Data.Maybe
import Core.Amount
import Data.Either.Utils(maybeToEither)
import qualified Data.Map as M
import Control.Monad.Except


capping :: [T.District] ->  [T.Bill] -> Either String [T.District]
capping  dists bills = traverse  (proportion bills)  dists 


proportion :: [T.Bill] -> T.District -> Either String T.District
proportion  bills dist =
  let
    maxCapFn     = maxCap dist
    totalFundFn  = totalDecidedFundByCategory dist bills
    specificFund = M.traverseWithKey (transform maxCapFn totalFundFn) $ T._distSpecifFunding  dist
  in
    T.District                         <$>
    pure (dist ^. T.distName )         <*>
    pure (dist ^. T.distAvailableFund) <*>
    pure (dist ^. T.distCategDefault)  <*>
    specificFund                       <*>
    pure (dist ^. T.distCaps)          <*>
    pure (dist ^. T.distRefundAmount)
  where
    transform  :: (T.Category -> Either String Amount)
               -> (T.Category -> Either String Amount)
               -> T.Bill
               -> Amount
               -> Either String Amount
    transform  capAmount  totalDecidedFund bill amount  = do
      category  <- lookupCategoryByBill (bill ^. T.billName) bills
      totalFund <- totalDecidedFund category
      maxCap    <- capAmount category
      if (totalFund > maxCap)
        then  return amount
        else  return $  ((amount  * maxCap) / totalFund)

-- Total decided or planned fund for a given category in a given district
totalDecidedFundByCategory ::
                              T.District
                           -> [T.Bill]
                           -> T.Category
                           -> Either String Amount
totalDecidedFundByCategory  category dist bills = undefined
--  M.foldl (+) 0 $  M.filter (predicate category) $ dist ^. T.distSpecifFunding
--  where 
--    predicate ::  T.Category -> (M.Map T.Bill v) -> Bool
--    predicate  category specFund =
--      isJust $ M.lookup (category) specFund

      
     --case isJust $ lookupCategoryByBill (specFund ^. T.billSpecificFundingBill) bills of
      --  True ->  fromJust ( lookupCategoryByBill (specFund ^. T.billSpecificFundingBill) bills ) == category
      -- False -> False

lookupCategoryByBill :: T.BillId -> [T.Bill] -> Either String T.Category
lookupCategoryByBill billId bills =
  case lookup  billId billPair of
    Nothing -> Left ""
    Just val -> Right val
  where
    billPair = zipWith (,) (map T._billName bills ) (map T._billCategory bills)
  

-- Cap for a given category
maxCap :: T.District -> T.Category -> Either String Amount
maxCap  dist category =
  maybeToEither "" $ M.lookup category $ dist ^.T.distCaps
