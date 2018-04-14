module Core.Capping
       (capping
       )where

import           Core.Types
import           Core.Amount(Amount)

import           Control.Lens((^.))
import           Data.Either.Utils(maybeToEither)

import qualified Data.Map as M


-- capping for all districts
capping :: [District] -> [Bill] -> Either LookupError [District]
capping  dists bills = traverse (proportion bills) dists 

-- reduce the fund for bills in case it violate the maximum cap decided for each bill category
proportion :: [Bill] -> District -> Either LookupError District
proportion  bills dist =
  let
    maxCapFn     = maxCap dist
    totalFundFn  = decidFundByCategory dist bills
    specificFund = M.traverseWithKey (transform maxCapFn totalFundFn) $ dist ^. distSpecifFunding
  in
    District                         <$>
    pure (dist ^. distName )         <*>
    pure (dist ^. distAvailableFund) <*>
    pure (dist ^. distCategDefault)  <*>
    specificFund                     <*>
    pure (dist ^. distCaps)          <*>
    pure (dist ^. distRefundAmount)
  where
    transform  :: (Category -> Either LookupError Amount)
               -> (Category -> Either LookupError Amount)
               -> Bill
               -> Amount
               -> Either LookupError Amount
    transform capAmount totalDecidedFund bill amount = do
      category  <- lookupCategoryByBill (bill ^. billName) bills
      totalFund <- totalDecidedFund category
      maxCap    <- capAmount category
      if (totalFund > maxCap)
        then  return $ ((amount * maxCap) / totalFund)
        else  return amount

-- Total decided or planned fund for a given category in a given district
decidFundByCategory :: District
                    -> [Bill]
                    -> Category
                    -> Either LookupError Amount
decidFundByCategory dist bills category = undefined
  M.foldl (+) 0 $  M.filterWithKey (pred category) $ dist ^. distSpecifFunding
  where
    pred :: Category -> Bill -> Amount -> Bool
    pred category bill _ = category == bill ^. billCategory

-- retrive the category from a given bill 
lookupCategoryByBill :: BillId -> [Bill] -> Either LookupError Category
lookupCategoryByBill billId bills =
  maybeToEither err $ lookup billId billPair
  where
    billPair = zipWith (,) (map _billName bills ) (map _billCategory bills)
    err = BillMissing $ show billId

-- Retrieve the maximum cap by category for a give district
maxCap :: District -> Category -> Either LookupError Amount
maxCap dist category =
  maybeToEither err $ M.lookup category $ dist ^. distCaps
  where err = MaxCapMissing $ show category
