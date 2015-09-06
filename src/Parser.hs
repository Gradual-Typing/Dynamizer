{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-unused-do-bind #-}

-- TODO: add state for bound variables to check for unbound

module Parser (parser) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)

import L1

-- sorted
reservedNames :: [String]
reservedNames =
  ["%/","%<<","%>>","*","+","-",":","<","<=",
   "=",">",">=","binary-and","binary-or","gbox",
   "gbox-set!","gunbox","gvector","gvector-ref",
   "gvector-set!","if","lambda","let","letrec",
   "mbox","mbox-set!","munbox","mvector",
   "mvector-ref","mvector-set!","repeat",
   "timer-report","timer-start","timer-stop"]

-- Utilities

isReservedName :: String -> Bool
isReservedName = isReserved reservedNames

isReserved :: Ord a => [a] -> a -> Bool
isReserved names name
  = scan names
  where
    scan []       = False
    scan (r:rs)   = case compare r name of
                     LT  -> scan rs
                     EQ  -> True
                     GT  -> False

-- Expression Parsers

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

whitespace :: Parser ()
whitespace =
    choice [simpleWhitespace *> whitespace,lineComment *> whitespace,return ()]
  where
    lineComment = try (string ";;") *>
                  manyTill anyChar (void (char '\n') <|> eof)
    simpleWhitespace = void $ many1 (oneOf " \t\n")

integer :: Parser Integer
integer = do
  f <- negate <$ char '-' <|> id <$ char '+' <|> return id
  n <- read <$> many1 digit
  return $ f n

op2Parser :: String -> Operator -> Parser Exp
op2Parser s op = do
          try $ string s
          e1 <- expParser
          whitespace
          e2 <- expParser
          return $ Op op [e1, e2]

c1Parser :: String -> (L1 -> Exp) -> Parser Exp
c1Parser s op = op <$ try (string s) <*> expParser

c2Parser :: String -> (L1 -> L1 -> Exp) -> Parser Exp
c2Parser s op = op <$ try (string s) <*> expParser <* whitespace <*> expParser

c3Parser :: String -> (L1 -> L1 -> L1 -> Exp) -> Parser Exp
c3Parser s op = op <$ try (string s) <*> expParser <* whitespace
                <*> expParser <* whitespace <*> expParser
  
idParser :: Parser String
idParser = try $ do
  name <- (:) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_')
  if isReservedName name
    then unexpected ("reserved word " ++ show name)
    else return name

argParser :: Parser Arg
argParser = do
  char '['
  x <- idParser
  string " : "
  t <- typeParser
  char ']'
  return (x,t)

bindParser :: Parser Bind
bindParser = do
  char '[' <|> char '('
  x <- idParser
  whitespace
  char ':'
  whitespace
  t <- typeParser
  whitespace
  e <- expParser
  char ']' <|> char ')'
  return (x,t,e)

ifParser,varParser,appParser,opsParser,intParser,boolParser
  ,lambdaParser,letParser,letrecParser,grefParser,gderefParser
  ,grefsetParser,mrefParser,mderefParser,mrefsetParser,gvectParser
  ,gvectrefParser,gvectsetParser,mvectParser,mvectrefParser
  ,mvectsetParser,asParser,beginParser,repeatParser,timer
  ,unitParser:: Parser Exp

unitParser = Unit <$ try (string "()")

ifParser = do
  string "if "
  e1 <- expParser
  whitespace
  e2 <- expParser
  whitespace
  e3 <- expParser
  return $ If e1 e2 e3

varParser = Var <$> idParser

appParser = parens (App <$> expParser <* whitespace <*> sepEndBy expParser whitespace)

opsParser = op2Parser "+ " Plus
            <|> op2Parser "- " Minus
            <|> op2Parser "* " Mult
            <|> op2Parser "%/ " Div
            <|> op2Parser "= " Eq
            <|> op2Parser ">= " Ge
            <|> op2Parser "> " Gt
            <|> op2Parser "<= " Le
            <|> op2Parser "< " Lt
            <|> op2Parser "%>> " ShiftR
            <|> op2Parser "%<< " ShiftL
            <|> op2Parser "binary-and " BAnd
            <|> op2Parser "binary-or " BOr

intParser = N <$> try integer

boolParser = (\x -> B $ x == 't') <$ char '#' <*> (char 't' <|> char 'f')

lambdaParser = do
  try $ string "lambda ("
  args <- sepEndBy argParser whitespace
  string ") : "
  rt <- typeParser
  whitespace
  b <- expParser
  return $ Lam args b rt

letParser = Let <$ try (string "let (") <*> sepEndBy bindParser whitespace
            <* char ')' <* whitespace <*> expParser
letrecParser = Letrec <$ try (string "letrec (") <*> sepEndBy bindParser whitespace
               <* string ")" <* whitespace <*> expParser

grefParser = c1Parser "gbox " GRef
gderefParser = c1Parser "gunbox " GDeRef
grefsetParser = c2Parser "gbox-set! " GAssign
mrefParser = c1Parser "mbox " MRef
mderefParser = c1Parser "munbox " MDeRef
mrefsetParser = c2Parser "mbox-set! " MAssign
gvectParser = c2Parser "gvector " GVect
gvectrefParser = c2Parser "gvector-ref " GVectRef
gvectsetParser = c3Parser "gvector-set! " GVectSet
mvectParser = c2Parser "mvector " GVect
mvectrefParser = c2Parser "mvector-ref " MVectRef
mvectsetParser = c3Parser "mvector-set! " MVectSet

asParser = As <$ string ": " <*> expParser <* space <*> typeParser

beginParser = do
  try $ string "begin "
  es <- sepEndBy1 expParser whitespace
  return $ Begin (init es) $ last es

repeatParser = do
  try $ string "repeat "
  char '('
  x <- idParser
  whitespace
  start <- expParser
  whitespace
  end <- expParser
  char ')'
  b <- expParser
  return $ Repeat x start end b

timer = (TimerStart <$ string "timer-start")
        <|> (TimerStop <$ string "timer-stop")
        <|> (TimerReport <$ string "timer-report")

expParser :: Parser L1
expParser = (,) <$> getPosition
            <*> (intParser
                 <|> boolParser
                 <|> opsParser
                 <|> unitParser
                 <|> ifParser
                 <|> varParser
                 <|> lambdaParser
                 <|> grefParser
                 <|> gderefParser
                 <|> grefsetParser
                 <|> mrefParser
                 <|> mderefParser
                 <|> mrefsetParser
                 <|> gvectParser
                 <|> gvectrefParser
                 <|> gvectsetParser
                 <|> mvectParser
                 <|> mvectrefParser
                 <|> mvectsetParser
                 <|> letParser
                 <|> letrecParser
                 <|> asParser
                 <|> beginParser
                 <|> repeatParser
                 <|> timer
                 <|> try appParser)
            <|> parens expParser

-- Type Parsers

grefTyParser,mrefTyParser,gvectTyParser,mvectTyParser,intTyParser
  ,boolTyParser,dynTyParser,unitTyParser,funTyParser
  ,typeParser :: Parser Type

intTyParser = IntTy <$ try (string "Int")
boolTyParser = BoolTy <$ try (string "Bool")
dynTyParser = Dyn <$ try (string "Dyn")
unitTyParser = UnitTy <$ try (string "()")
funTyParser = do
  ts <- parens $ sepEndBy typeParser (string " -> ")
  return $ FunTy (init ts) (last ts)
  
grefTyParser = parens (GRefTy <$ try (string "GRef ") <*> typeParser)
mrefTyParser = parens (MRefTy <$ try (string "MRef ") <*> typeParser)
gvectTyParser = parens (GVectTy <$ try (string "GVect ") <*> typeParser)
mvectTyParser = parens (MVectTy <$ try (string "MVect ") <*> typeParser)

typeParser = intTyParser
             <|> boolTyParser
             <|> dynTyParser
             <|> unitTyParser
             <|> funTyParser
             <|> grefTyParser
             <|> mrefTyParser
             <|> gvectTyParser
             <|> mvectTyParser

schmlParser :: Parser L1
schmlParser = id <$ whitespace <*> expParser <* whitespace <* eof

parser :: String -> Either ParseError L1
parser = parse schmlParser ""
  
-- main :: IO ()            
-- main = parseTest schmlParser "(letrec ([x : (Int Int -> Int) (lambda ([x : Int] [y : Int]) : Int (* x (: -2 Int)))] [y : Bool #f]) (let ([z : Int 5] [t : () ()]) (if (> 3 1) (x z) 0)))"
