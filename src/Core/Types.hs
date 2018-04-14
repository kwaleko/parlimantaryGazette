{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Types where

import                Control.Lens
import                Data.Text
import                Data.Map 
import                GHC.Generics


import                Core.Amount

type Error    = Text
type BillId   = Text 

data JData = JData
  {
     _bills :: [Bill]
    ,_districts :: [District]
  } deriving(Eq,Show,Generic)

data District = District
  {
     _distName             :: Text
    ,_distAvailableFund    :: Amount
    ,_distCategDefault     :: Map Category Amount
    ,_distSpecifFunding    :: Map Bill     Amount  --  district wishes to pay
    ,_distCaps             :: Map Category Amount 
    ,_distRefundAmount     :: Maybe Amount
  } deriving (Show,Eq,Generic,Ord)

data Bill = Bill
  {
     _billName         :: BillId
    ,_billCategory     :: Category
    ,_billAmount       :: Amount
    ,_billReceivedFund :: Maybe Amount 
  }deriving(Eq,Show,Generic,Ord)

data LookupError = BillDoesNotExists
                 | MaxCapMissing Text
                 | BillMissing   Text
                 deriving Show

data Category =  Category Text
              deriving (Eq,Show,Generic,Ord)

data JSONError = JSONErrorWrongFormat Text
               | JSONErrorReadingFile
               deriving (Eq,Show,Generic)

makeLenses ''Bill
makeLenses ''District
makeLenses ''JData
