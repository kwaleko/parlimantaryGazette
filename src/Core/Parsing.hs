{-# LANGUAGE OverloadedStrings #-}
module Core.Parsing where

import Control.Monad(when)
import Data.Aeson(FromJSON(..)
                 ,ToJSON(..)
                 ,withObject
                 ,(.:))

import Core.Types
import Core.PlannedFund

import Data.Aeson.Types


instance FromJSON Cap where
  parseJSON = withObject "cap" $ \o -> do
    category <- o .: "category"
    amount <- o .: "amount"
    return $ Cap category amount

parseJData :: Value -> Parser JData
parseJData = withObject "JData" $ \o -> do
  bills <- o .: "bills"
  districts <- o .: "districts"
--  validate bills districts
  return  $ JData bills districts

-- health check of the data provided
-- for instance, if there is a bill that does not have a matching category in CategoryDefaultFunding
-- then the parse will fail with an appropriate error
{-validate :: [Bill] -> [District] -> Parser Bool
validate bills districts = do
  let res =  
  return True
  where
    helperFunc :: BillId -> [Bill] -> Maybe Category
    helperFunc billid bills = lookupCategoryByBill billid bills
-}
-- the Bill Amount could not be negative
-- so the parsing behavior is altered
parseCategoryDefaultFunding :: Value -> Parser CategoryDefaultFunding
parseCategoryDefaultFunding =  withObject "categoryDefaultFunding" $ \o -> do
    category <- o .: "category"
    amount <- o .: "amount"
    when (amount < 0) $
      fail $ "The default funding for the category" ++ category ++ "should be nonnegative number"
    return $ CategoryDefaultFunding (Category category) amount

-- Bill Name cannot be Empty
parseBill :: Value -> Parser Bill
parseBill = withObject "bill" $ \o -> do
    name <- o .: "name"
    category <- o .: "category"
    amount <- o .: "amount"
    when (amount < 0) $
      fail $ "the fund needed to implement the bill ("++ show name ++  ") should be nonnegative number"
    return $ Bill name category amount

 
instance FromJSON BillSpecificFunding where
  parseJSON = withObject "billSpecificFunding" $ \o -> do
    bill <- o .: "bill"
    amount <- o .: "amount"
    when (amount < 0) $
      fail $   "The fund Specified for the bill (" ++ show bill ++" ) should be positive" 
    return $ BillSpecificFunding bill amount



instance FromJSON District where
  parseJSON = withObject "district" $ \o -> do
    name <- o .: "name"
    availableFunds <- o .: "availableFunds"
    categoryDefaultFunding <- o .: "categoryDefaultFunding"
    billSpecificFunding <- o .: "billSpecificFunding"
    caps <- o .: "caps"
    return $ District name availableFunds categoryDefaultFunding billSpecificFunding caps

instance FromJSON Bill where
  parseJSON = parseBill

instance FromJSON JData where
  parseJSON = parseJData

instance FromJSON CategoryDefaultFunding where
  parseJSON = parseCategoryDefaultFunding

instance FromJSON Category

instance ToJSON JData
instance ToJSON District
instance ToJSON CategoryDefaultFunding
instance ToJSON BillSpecificFunding
instance ToJSON Category
instance ToJSON Cap
instance ToJSON Bill
instance ToJSON JSONError
