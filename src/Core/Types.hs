{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Types where

import Control.Lens
import Data.Text
import Data.Aeson
import GHC.Generics


-------------------------------  P O S I T I V E    N U M B E R -----------------------------------------------------------
-- THERE IS NOTHING IN THE TYPE THAT PROVE/GURANTEE THAT THE NUMBER SHOULD BE POSITIVE, THIS IS A BAD DESIGN CHOICE CHOICE
-- AND I AM AWARE OF IT, FOR NOW I HAVE UPDATED THE JSON PARSER TO FAIL IF IT CATCHES A NON POSITIVE NUMBER
-- HOWEVER, IT IS BETTER TO EXPRESS IT IN THE TYPE SYSTEM...
----------------------------------------------------------------------------------------------------------------------------


type Error    = String
type BillId = String 

data ActualFund = ActualFund
  {
     obillName :: String
    ,obillActualFund :: Int
    ,obillFullyFunded :: Bool
  } deriving(Eq,Show,Generic)

data ODistrict = ODistrict
  {
     odistrictName :: String
    ,collectedTax :: Int
    ,refundAmount :: Int
    ,actualFund :: [ActualFund]
  }

data OBill = OBill
  {
    oBillName :: String
    ,oBillCategory :: Category
    ,oBillAmount :: Int
    ,oActualFund  :: Int
    
  }

data JData = JData
  {
     _bills :: [Bill]
    ,_districts :: [District]
  } deriving(Eq,Show,Generic)

data District = District
  {
     _distName             :: String
    ,_distAvailableFund    :: Int
    ,_distCategDefault     :: [CategoryDefaultFunding]
    ,_distBillSpecificFund :: [BillSpecificFunding] --  district wishes to pay
    ,_distCaps             :: [Cap]
  } deriving (Show,Eq,Generic)

data Bill = Bill
  {
     _billName     :: BillId
    ,_billCategory :: Category
    ,_billAmount   :: Int 
  }deriving (Eq,Show,Generic)

data BillSpecificFunding = BillSpecificFunding
  {
     _billSpecificFundingBill   :: BillId
    ,_billSpecificFundingAmount :: Int
  } deriving(Eq,Show,Generic)


data Cap = Cap
  {
     _capCategory :: Category
    ,_capAmount   :: Int 
  }deriving(Eq,Show,Generic)

data CategoryDefaultFunding = CategoryDefaultFunding
  {
     _categoryDefaultFundingCategory :: Category
    ,_categoryDefaultFundingAmount   :: Int
  } deriving(Eq,Show,Generic)

data Category =  Category String
  deriving(Eq,Show,Generic)

data JSONError = 
    JSONErrorWrongFormat String
  | JSONErrorReadingFile
  deriving(Eq,Show,Generic)


makeLenses ''Bill
makeLenses ''District
makeLenses ''BillSpecificFunding
makeLenses ''Cap
makeLenses ''CategoryDefaultFunding
makeLenses ''JData





