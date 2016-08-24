{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P as) (P bs) = (==) as bs
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P as)
      | length as == 0 = "0"
      | length as == 1 && as == [0] = "0"
      | otherwise = intercalate " + " $ filter (/= "") $ map getTerm $ reverse $ zip as [0..]
      where getTerm :: (Num a, Eq a, Show a) => (a, Integer) -> String
            getTerm (c, e)
              | c == 0 = ""
              | e == 0 = show c
              | otherwise = coefficient ++ exponent'
              where coefficient = case c of 1  -> ""
                                            -1 -> "-"
                                            _  -> show c
                    exponent' = case e of 1 -> "x"
                                          _ -> "x^" ++ show e

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P as) (P bs) = P $ zipWith (+) padded_as padded_bs
  where padded_as = if (length as) < (length bs) then (as ++ zero_pad) else as
        padded_bs = if (length as) > (length bs) then (bs ++ zero_pad) else bs
        zero_pad = flip replicate 0 $ abs $ (-) (length as) (length bs)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P as) (P bs) = sum polynomials
  where zero_pads = map (flip replicate 0) [0..]
        multiplied_values = map (\a -> map (*a) bs) as
        polynomials = map (P) $ zipWith (++) zero_pads multiplied_values

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate = (times) (P [-1])
    fromInteger = P . (:[]) . fromInteger
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P as) value = sum $ zipWith (\a b -> a * b) as (iterate (* value) 1)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f
      | n == 0 = f
      | otherwise = nderiv (n - 1) (deriv f)

-- Exercise 9 -----------------------------------------

instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P as) = P $ drop 1 $ zipWith (*) as [0..]

