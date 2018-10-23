{- Assignment 3 Extra Credit
 - Name: TODO add full name
 - Date: TODO add of completion
 -}
module Assign_3_ExtraCredit where

macid = "TODO: put your mac id here"

data PolyAlt a = Monomial a Integer
               | SumAlt (PolyAlt a) (PolyAlt a)
  deriving Show
