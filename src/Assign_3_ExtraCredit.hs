{- Assignment 3 Extra Credit
 - Name: Omar Alkersh
 - Date: 28/10/18
 -}
module Assign_3_ExtraCredit where
import Assign_3

macid = "oalkresh"

data PolyAlt a = Monomial a Integer
               | SumAlt (PolyAlt a) (PolyAlt a)
  deriving Show

{-------------------------------------------------------------------
  -Takes a polynomial function, PolyAlt, and a number, Num a => n
  -and returns result from subbing n into the function
  -example:
  -(Monomial 5 2) => 5*x^2
  -n=5 => 5(5)^2 = 125
  --}
--TODO: replace tests
{- -----------------------------------------------------------------
  - Test Cases
  - -----------------------------------------------------------------
  - polyDeriv
  - -----------------------------------------------------------------
  - - Function: polyDeriv
  - - Test Case Number: 1-A
  - - Input: X
  - - Expected Output: Coef 1
  - - Acutal Output: Coef 1
  - -----------------------------------------------------------------
  -}
polyAltValue:: Num a => PolyAlt a -> a -> a
polyAltValue (Monomial x y) n = x*n ^ abs y
polyAltValue (SumAlt x y) n = polyAltValue x n + polyAltValue y n

polyAltDegree:: Num a => PolyAlt a ->  Integer
polyAltDegree (Monomial _ y) = y
polyAltDegree (SumAlt x y) = max (polyAltDegree x) (polyAltDegree y)

polyAltDeriv :: Num a => PolyAlt a -> PolyAlt a
polyAltDeriv (Monomial x y) | y == 0 = Monomial 0 0
                            | otherwise = Monomial (x * fromInteger y) (y-1)
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

polyAltToPoly :: (Num a, Eq a) => PolyAlt a -> Poly a
polyAltToPoly (Monomial x y) | y == 0 = Coef x
                             | y > 0 = Prod X (polyAltToPoly (Monomial x (y-1)))
polyAltToPoly (SumAlt x y) = Sum (polyAltToPoly x) (polyAltToPoly y)

(-->) :: Bool -> Bool -> Bool
a --> b = not a || b

--tests polyAltToPoly function
polyAltToPolyCheck1 :: (Num a, Eq a) =>(a, Integer, a)-> Bool
polyAltToPolyCheck2 :: (Num a, Eq a) => (a, Integer) -> (a, Integer) -> a -> Bool
polyAltToPolyCheck1 (c,e,n) = let
  p = Monomial c e
  in (e > 0) --> (polyAltValue p n == polyValue (polyAltToPoly p) n)
polyAltToPolyCheck2 (c1,e1) (c2,e2) n =let
  p = SumAlt (Monomial c1 e1) (Monomial c2 e2)
  in ((e1>0) && (e2>0)) --> (polyAltValue p n == polyValue (polyAltToPoly p) n)

--tests polyToPolyAlt function
polyToPolyAltCheck1 :: (Num a, Eq a) => a -> Integer -> a -> Bool
polyToPolyAltCheck2 :: (Num a, Eq a) => (a, Integer) -> (a, Integer) -> a -> Bool
polyToPolyAltCheck1 c e n = let
  p = Monomial c e
  in (e>0) --> (polyAltValue(polyToPolyAlt(polyAltToPoly p)) n == polyAltValue p n)
polyToPolyAltCheck2 (c1,e1) (c2,e2) n = let
  p = SumAlt (Monomial c1 e1) (Monomial c2 e2)
  in (e1>0 && e2>0) --> ( polyAltValue(polyToPolyAlt(polyAltToPoly p)) n == polyAltValue p n)
