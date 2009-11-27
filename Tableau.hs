module Tableau ( Tableau,
                 readTableau,
                 createTableau,
                 findCounterExample
               ) where

import Predicate
import Prelude as P
import Data.Set as S
import Data.List as L
import Data.Maybe
import Control.Monad

data Tableau = Tableau { leftTableau :: [Predicate],
                         rightTableau :: [Predicate],
                         leftValues :: Set Identifier,
                         rightValues :: Set Identifier
                       }

instance Show Tableau where
    show = showTableau

showTableau :: Tableau -> String
showTableau (Tableau l r lv rv) = (side l lv) ++ " . " ++ (side r rv)
    where side x xv = intercalate ", " $ (P.map show x) ++ (toList xv)

readTableau :: [String] -> [String] -> Tableau
readTableau left right = createTableau (readAll left) (readAll right)
    where readAll = fromMaybe [] . sequence . L.map readPredicate

createTableau :: [Predicate] -> [Predicate] -> Tableau
createTableau left right = Tableau left right S.empty S.empty

findCounterExample :: Tableau -> Maybe Tableau
findCounterExample t@(Tableau l v lv rv)
    | any (`member` rv) (toList lv) = Nothing
    | P.null l && P.null v = Just t
    | otherwise = foldr mplus Nothing $ P.map findCounterExample $ simplifyTableau t

simplifyTableau :: Tableau -> [Tableau]
simplifyTableau (Tableau [] [] lv rv) = []
simplifyTableau (Tableau [] r lv rv) = simplifyTableauRight (Tableau [] (tail r) lv rv) (head r)
simplifyTableau (Tableau l r lv rv) = simplifyTableauLeft (Tableau (tail l) r lv rv) (head l)

simplifyTableauLeft :: Tableau -> Predicate -> [Tableau]
simplifyTableauLeft (Tableau l r lv rv) p =
    case p of (Value x) -> [Tableau l r (S.insert x lv) rv]
              (Not x) -> [Tableau l (x:r) lv rv]
              (And x y) -> [Tableau (x:y:l) r lv rv]
              (Or x y) -> [Tableau (x:l) r lv rv, Tableau (y:l) r lv rv]

simplifyTableauRight :: Tableau -> Predicate -> [Tableau]
simplifyTableauRight (Tableau l r lv rv) p =
    case p of (Value x) -> [Tableau l r lv (S.insert x rv)]
              (Not x) -> [Tableau (x:l) r lv rv]
              (And x y) -> [Tableau l (x:r) lv rv, Tableau l (y:r) lv rv]
              (Or x y) -> [Tableau l (x:y:r) lv rv]
