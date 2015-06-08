module SwapIFC.Primitives where

import SwapIFC.Types

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Primitives for flows which contains Num values

infixl 7 .*.
infixl 6 .+., .-.

-- | Addition of two flows which contains Num values
(.+.) :: (Num a, CombineFlow t1 t2 t3) => Flow t1 a -> Flow t2 a -> Flow t3 a
(.+.) = calcNumFlow (+)

-- | Multiplication of two flows which contains Num values
(.*.) :: (Num a, CombineFlow t1 t2 t3) => Flow t1 a -> Flow t2 a -> Flow t3 a
(.*.) = calcNumFlow (*)

-- | Subtraction of two flows which contains Num values
(.-.) :: (Num a, CombineFlow t1 t2 t3) => Flow t1 a -> Flow t2 a -> Flow t3 a
(.-.) = calcNumFlow (-)

-- | Unary negation for Flow
fNeg :: (Num a, ValidFlow t1 t2) => Flow t1 a -> Flow t2 a
fNeg = applyNumFlow negate

-- | Unary absolute value for Flow
fAbs :: (Num a, ValidFlow t1 t2) => Flow t1 a -> Flow t2 a
fAbs = applyNumFlow abs

-- | Unary signum for Flow
fSig :: (Num a, ValidFlow t1 t2) => Flow t1 a -> Flow t2 a
fSig = applyNumFlow signum

calcNumFlow :: (Num a, CombineFlow t1 t2 t3) => (a -> a -> a)
                                             -> Flow t1 a
                                             -> Flow t2 a
                                             -> Flow t3 a
calcNumFlow op (Flow ioa1) (Flow ioa2) = Flow $ do
  a1 <- ioa1
  a2 <- ioa2
  return $ op a1 a2

applyNumFlow :: (Num a, ValidFlow t1 t2) => (a -> a)
                                         -> Flow t1 a
                                         -> Flow t2 a
applyNumFlow f (Flow ioa) = Flow $ do
  a <- ioa
  return $ f a

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Primitives for flows which contains Fractional values
infixl 7 ./.

-- | Division of two flows which contains Num values
(./.) :: (Fractional a, CombineFlow t1 t2 t3) => Flow t1 a -> Flow t2 a -> Flow t3 a
(Flow ioa1) ./. (Flow ioa2) = Flow $ do
  a1 <- ioa1
  a2 <- ioa2
  return $ a1 / a2

-- | Unary recip for Flow
fRecip :: (Fractional a, ValidFlow t1 t2) => Flow t1 a -> Flow t2 a
fRecip (Flow ioa) = Flow $ do
  a <- ioa
  return $ recip a

-- | Unary fromRational for Flow
fFromRational :: (Fractional a, ValidFlow t1 t2) => Flow t1 Rational -> Flow t2 a
fFromRational (Flow ior) = Flow $ do
  r <- ior
  return $ fromRational r

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
