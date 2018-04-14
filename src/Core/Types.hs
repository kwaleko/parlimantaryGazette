{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Types where

import Control.Lens
import Data.Text
import Data.Aeson
import GHC.Generics
import Data.Map 

import Core.Amount


-------------------------------  P O S I T I V E    N U M B E R -----------------------------------------------------------
-- THERE IS NOTHING IN THE TYPE THAT PROVE/GURANTEE THAT THE NUMBER SHOULD BE POSITIVE, THIS IS A BAD DESIGN CHOICE CHOICE
-- AND I AM AWARE OF IT, FOR NOW I HAVE UPDATED THE JSON PARSER TO FAIL IF IT CATCHES A NON POSITIVE NUMBER
-- HOWEVER, IT IS BETTER TO EXPRESS IT IN THE TYPE SYSTEM...
----------------------------------------------------------------------------------------------------------------------------


type Error    = String
type BillId = String 


data JData = JData
  {
     _bills :: [Bill]
    ,_districts :: [District]
  } deriving(Eq,Show,Generic)

data District = District
  {
     _distName             :: String
    ,_distAvailableFund    :: Amount
    ,_distCategDefault     :: Map Category Amount
    ,_distSpecifFunding    :: Map Bill Amount  --  district wishes to pay
    ,_distCaps             :: Map Category Amount 
    ,_distRefundAmount     :: Maybe Amount
  } deriving (Show,Eq,Generic,Ord)


data Bill = Bill
  {
     _billName         :: BillId
    ,_billCategory     :: Category
    ,_billAmount       :: Amount
    ,_billReceivedFund :: Maybe Amount 
  }deriving (Eq,Show,Generic,Ord)

data LookupError = BillDoesNotExists
                 | MaxCapMissing String
                 | BillMissing   String
  deriving Show

data Category =  Category String
  deriving(Eq,Show,Generic,Ord)

data JSONError = 
    JSONErrorWrongFormat String
  | JSONErrorReadingFile
  deriving(Eq,Show,Generic)

makeLenses ''Bill
makeLenses ''District
makeLenses ''JData






