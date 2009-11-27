module Predicate ( Identifier,
                   Predicate(..),
                   readPredicate
                 ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

type Identifier = String

data Predicate = Value Identifier
               | Not Predicate
               | And Predicate Predicate
               | Or Predicate Predicate
               | Implicates Predicate Predicate
               deriving (Eq, Ord)

instance Show Predicate where
    show = showPredicate

showPredicate :: Predicate -> String
showPredicate (Value x) = x
showPredicate (Not x) = "~" ++ (showPredicate x)
showPredicate (And x y) = "(" ++ (showPredicate x) ++ " && " ++ (showPredicate y) ++ ")"
showPredicate (Or x y) = "(" ++ (showPredicate x) ++ " || " ++ (showPredicate y) ++ ")"
showPredicate (Implicates x y) = "(" ++ (showPredicate x) ++ " -> " ++ (showPredicate y) ++ ")"

readPredicate :: String -> Maybe Predicate
readPredicate str = let r = runParser parsePredicate () "input" str
                    in case r of Left _ -> Nothing
                                 Right p -> Just p

parsePredicate :: Parser Predicate
parsePredicate = buildExpressionParser table parsePredicateTerm
             <?> "expression"

table :: OperatorTable Char () Predicate
table = [ [unary "~" Not],
          [binary "&&" And, binary "||" Or],
          [binary "->" Implicates]
        ]
    where unary  str f = Prefix $ string str >> return f
          binary str f = Infix (string str >> return f) AssocLeft

parsePredicateTerm = 
    whiteSpace >> predicate >>= \p -> whiteSpace >> return p
    where predicate = parens <|> value <?> "predicate"
          whiteSpace = skipMany $ char ' '
          parens = do { char '('; e <- parsePredicate;  char ')'; return e }
          value = (many1 alphaNum) >>= return . Value
