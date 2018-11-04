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
 - polyAltValue
 -------------------------------------------------------------------
 - Takes a polynomial function, PolyAlt, and a number, Num a => n
 - and returns result from subbing n into the function
 - example:
 - (Monomial 5 2) => 5*x^2
 - n=5 => 5(5)^2 = 125
 -}

polyAltValue:: Num a => PolyAlt a -> a -> a
polyAltValue (Monomial x y) n = x*n ^ abs y
polyAltValue (SumAlt x y) n = polyAltValue x n + polyAltValue y n

{-------------------------------------------------------------------
 - polyAltDegree
 -------------------------------------------------------------------
 - Finds the highest exponent in a PolyAlt a polyValue
 - For any Monomial a b the degree is b
 - for any SumAlt x y the degree is equal to highest between x and y
 - where x amd y are PolyAlt
 -}

polyAltDegree:: Num a => PolyAlt a ->  Integer
polyAltDegree (Monomial _ y) = y
polyAltDegree (SumAlt x y) = max (polyAltDegree x) (polyAltDegree y)

{-------------------------------------------------------------------
 - polyAltDeriv
 -------------------------------------------------------------------
 - Uses dereivation rules to find the derivative of PolyAlt data
 - for Monomial a b it uses the power rule
 - for SumAlt x y the funstion employs recursion
 -}

polyAltDeriv :: Num a => PolyAlt a -> PolyAlt a
polyAltDeriv (Monomial x y) | y == 0 = Monomial 0 0
                            | otherwise = Monomial (x * fromInteger y) (y-1)
polyAltDeriv (SumAlt x y) = SumAlt (polyAltDeriv x) (polyAltDeriv y)

{-------------------------------------------------------------------
 - polyAltProd
 -------------------------------------------------------------------
 - Miltiplies two PolyAlt values using aithmatic laws:
 - x*y => x*y
 - x(a+b) => a*x + b*x
 - (a+b)(x+y) => a*x + a*y + b*x + b*y
 - This function can emplyes recursion where Monomial a b is the simplest case
 -}

polyAltProd :: Num a => PolyAlt a -> PolyAlt a -> PolyAlt a
polyAltProd (Monomial x y) (Monomial a b) = Monomial (x*a) (y+b)
polyAltProd (Monomial x y) (SumAlt a b) = SumAlt (polyAltProd (Monomial x y) a) (polyAltProd (Monomial x y) b)
polyAltProd (SumAlt a b) (Monomial x y) = SumAlt (polyAltProd (Monomial x y) a) (polyAltProd (Monomial x y) b)
polyAltProd (SumAlt a b) (SumAlt x y) = SumAlt (SumAlt (polyAltProd a x) (polyAltProd a y)) (SumAlt (polyAltProd b x) (polyAltProd b y))

{-------------------------------------------------------------------
 - polyAltNewton
 -------------------------------------------------------------------
 - estimates the root of the equation using Newton's method -> http://tutorial.math.lamar.edu/Classes/CalcI/NewtonsMethod.aspx
 - In short, newtons method starts with a point on the graph, x1, and uses the tangent of the graph on that point to estimate the second point, x2, which is where the tangent corsses the x-axis, the same procedure is repeated using the second point until the difference between xn and xn-1 is <= tolerance, which predefined
 - The original point and the function are already given, s and p respectively
 - the second point is found using y=mx+b general equation
 - m, gradient is found using polyAltDeriv and polyAltValue
 - b is found by rearranging the y=mx+b
 - the y is found using polyAltValue
 - the difference between s and x2 is compared to the tolerance, t
 - if it is less than the tolerance the x2 is returned
 - other the whole procedure is repeated using x2 as the starting point
 -}

polyAltNewton :: (Fractional a, Ord a) => PolyAlt a -> a -> a -> a
polyAltNewton p s t = let
  m = polyAltValue (polyAltDeriv p) s
  y = polyAltValue p s
  b = y - m*s
  x2 = -(b/m)
  in if abs(x2-s) <  t
    then x2
    else polyAltNewton p x2 t

{-------------------------------------------------------------------
 - polyToPolyAlt
 -------------------------------------------------------------------
 - turns Poly, defined in Assign_3, value to PolyAlt
 - Uses recursio where appropriate, def 3-4
 - base cases defined in def 1-2
 - uses polyAltProd in def.4 to find the product of x and y
 -}

polyToPolyAlt :: (Num a, Eq a) => Poly a -> PolyAlt a
polyToPolyAlt X = Monomial 1 1 -- def.1
polyToPolyAlt (Coef x) = Monomial x 0 --def.2
polyToPolyAlt (Sum x y) = SumAlt (polyToPolyAlt x) (polyToPolyAlt y) -- def.3
polyToPolyAlt (Prod x y) = polyAltProd (polyToPolyAlt x) (polyToPolyAlt y) --def.

{-------------------------------------------------------------------
 - polyAltToPoly
 -------------------------------------------------------------------
 - Turns PolyAlt values to Poly, defined in Assign_3.
 - Uses recursion in def.1.3 and def.2
 - base casses defined in def.1.1-2
 -}

polyAltToPoly :: (Num a, Eq a) => PolyAlt a -> Poly a
polyAltToPoly (Monomial x y) | x == 0 = Coef 0 --def.1.1
                             | y == 0 = Coef x --def.1.2
                             | y > 0  = Prod X (polyAltToPoly (Monomial x (y-1))) --def.1.3
polyAltToPoly (SumAlt x y) = Sum (polyAltToPoly x) (polyAltToPoly y) --def.2

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
