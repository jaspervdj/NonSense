module Predicate where

type Identifier = String

data Predicate = Value Identifier
               | And Predicate Predicate
               | Or Predicate Predicate
               | Not Predicate
               deriving (Eq, Ord)

instance Show Predicate where
    show = showPredicate

showPredicate :: Predicate -> String
showPredicate (Value x) = x
showPredicate (And x y) = "(" ++ (showPredicate x) ++ " && " ++ (showPredicate y) ++ ")"
showPredicate (Or x y) = "(" ++ (showPredicate x) ++ " || " ++ (showPredicate y) ++ ")"
showPredicate (Not x) = "~" ++ (showPredicate x)
