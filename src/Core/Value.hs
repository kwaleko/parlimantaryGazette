{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Value where


newtype Value = Value (Either Error Int) deriving Eq

data Error = NegativeNumber deriving Eq

instance Num Value where
  Value a + Value b = Value $ (+) <$> a <*> b
  Value a - Value b = Value $ (-) <$> a <*> b
  Value a * Value b = Value $ (*) <$> a <*> b

instance Fractional Value where
  Value (Left err) / _                = Value (Left err)
  _                / Value (Left err) = Value (Left err)
  Value (Right a)  / Value (Right b)  = Value $
    if a `rem` b == 0 || 2 * (a `rem` b ) < b
                then Right $  a `div` b
                else Right $ (+1) $ a `div` b


instance Show Value where
  show value = case value of
    Value (Left NegativeNumber) -> "Number is negative"
    Value (Right a)             -> show a

   -- | nb `rem` divisor == 0        =  nb `div` divisor
  -- | (2 * (nb `rem` divisor)) >= divisor  =(+) 1 $ nb `div` divisor
 -- | True                         =   nb `div` divisor
