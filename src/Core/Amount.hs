{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Value
  (Value(..)
  ,Error(..)
  )where


newtype Value = Value (Either Error Int) deriving Eq

data Error = NegativeNumber
           | CouldNotBeFractional
           deriving Eq

instance Num Value where
  Value a + Value b     = Value $ (+) <$> a <*> b
  Value a - Value b     = Value $ (-) <$> a <*> b
  Value a * Value b     = Value $ (*) <$> a <*> b
  abs (Value a)         = Value $ abs <$> a
  signum (Value a)       = Value $ signum <$> a
  fromInteger a = Value $ pure  $ fromInteger a

instance Fractional Value where
  Value (Left err) / _                = Value (Left err)
  _                / Value (Left err) = Value (Left err)
  Value (Right a)  / Value (Right b)  = Value $
    if a `rem` b == 0 || 2 * (a `rem` b ) < b
                then Right $  a `div` b
                else Right $ (+1) $ a `div` b
  fromRational a = Value $ Left CouldNotBeFractional


instance Show Value where
  show value = case value of
    Value (Left NegativeNumber) -> "Number is negative"
    Value (Left CouldNotBeFractional) -> "could not be fractional"
    Value (Right a)             -> show a


