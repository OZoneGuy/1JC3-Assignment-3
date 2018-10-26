{- Assignment 3 Extra Credit
 - Name: Omar Alkersh
 - Date: TODO add of completion
 -}
module Assign_3_ExtraCredit where
import Assign_3

macid = "oalkresh"

data PolyAlt a = Monomial a Integer
               | SumAlt (PolyAlt a) (PolyAlt a)
  deriving Show

polyAltValue:: Num a => PolyAlt a -> a -> a
polyAltValue (Monomial x y) n = x*n ^ abs y
polyAltValue (SumAlt x y) n = polyAltValue x n + polyAltValue y n

polyAltDegree:: Num a => PolyAlt a ->  Integer
polyAltDegree (Monomial _ y) = y
polyAltDegree (SumAlt x y) = max (polyAltDegree x) (polyAltDegree y)

polyAltDeriv :: Num a => PolyAlt a -> PolyAlt a
polyAltDeriv (Monomial x y) | y == 0 = Monomial 0 0
                            | otherwise = Monomial (x* fromInteger y) (y-1)
polyAltDeriv (SumAlt x y) = SumAlt (polyAltDeriv x) (polyAltDeriv y)

polyAltProd :: Num a => PolyAlt a -> PolyAlt a -> PolyAlt a
polyAltProd (Monomial x y) (Monomial a b) = Monomial (x*a) (y+b)
polyAltProd (Monomial x y) (SumAlt a b) = SumAlt (polyAltProd (Monomial x y) a) (polyAltProd (Monomial x y) b)
polyAltProd (SumAlt a b) (Monomial x y) = SumAlt (polyAltProd (Monomial x y) a) (polyAltProd (Monomial x y) b)
polyAltProd (SumAlt a b) (SumAlt x y) = SumAlt (SumAlt (polyAltProd a x) (polyAltProd a y)) (SumAlt (polyAltProd b x) (polyAltProd b y))

polyAltNewton :: (Fractional a, Ord a) => PolyAlt a -> a -> a -> a
polyAltNewton p s t = let
  m = polyAltValue (polyAltDeriv p) s
  y = polyAltValue p s
  b = y - m*s
  x2 = -(b/m)
  in if abs(x2-s) <  t
    then x2
    else polyAltNewton p x2 t

polyToPolyAlt :: (Num a, Eq a) => Poly a -> PolyAlt a
polyToPolyAlt X = Monomial 1 1
polyToPolyAlt (Coef x) = Monomial x 0
polyToPolyAlt (Sum x y) = SumAlt (polyToPolyAlt x) (polyToPolyAlt y)
polyToPolyAlt (Prod x y) = polyAltProd (polyToPolyAlt x) (polyToPolyAlt y)

--TODO implement polyAltToPoly
