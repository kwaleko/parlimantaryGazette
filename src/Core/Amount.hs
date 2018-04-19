{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core.Amount
  (Amount
  ,Error(..)
  ,mkAmount
  )where


newtype Amount = A (Either Error Int) deriving (Eq,Ord)

data Error = NegativeNumber
           | CouldNotBeFractional
           deriving (Eq,Ord)

mkAmount ::  Int -> Amount
mkAmount a = if a < 0
                then A $ Left NegativeNumber
                else A $ Right a

instance Num Amount where
  A a + A b     = A $ (+) <$> a <*> b
  A a - A b     = A $ (-) <$> a <*> b
  A a * A b     = A $ (*) <$> a <*> b
  abs (A a)     = A $ abs <$> a
  signum (A a)  = A $ signum <$> a
  fromInteger a = A $ pure  $ fromInteger a

instance Fractional Amount where
  A (Left err) / _                = A (Left err)
  _                / A (Left err) = A (Left err)
  A (Right a)  / A (Right b)  = A $
    if a `rem` b == 0 || 2 * (a `rem` b ) < b
                then Right $  a `div` b
                else Right $ (+1) $ a `div` b
  fromRational a = A $ Left CouldNotBeFractional


instance Show Amount where
  show value = case value of
    A (Left NegativeNumber)       -> "Number is negative"
    A (Left CouldNotBeFractional) -> "could not be fractional"
    A (Right a)                   -> show a


