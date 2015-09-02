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

import Syntax

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

op2Parser :: String -> Operator -> Parser Exp1
op2Parser s op = do
          try $ string s
          e1 <- expParser
          space
          e2 <- expParser
          return $ Op1 op [e1, e2]

c1Parser :: String -> (L1 -> Exp1) -> Parser Exp1
c1Parser s op = op <$ try (string s) <*> expParser

c2Parser :: String -> (L1 -> L1 -> Exp1) -> Parser Exp1
c2Parser s op = op <$ try (string s) <*> expParser <* space <*> expParser

c3Parser :: String -> (L1 -> L1 -> L1 -> Exp1) -> Parser Exp1
c3Parser s op = op <$ try (string s) <*> expParser <* space
                <*> expParser <* space <*> expParser
  
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
  string " : "
  t <- typeParser
  space
  e <- expParser
  char ']' <|> char ')'
  return (x,t,e)

ifParser,varParser,appParser,opsParser,intParser,boolParser
  ,lambdaParser,letParser,letrecParser,grefParser,gderefParser
  ,grefsetParser,mrefParser,mderefParser,mrefsetParser,gvectParser
  ,gvectrefParser,gvectsetParser,mvectParser,mvectrefParser
  ,mvectsetParser,asParser,beginParser,repeatParser,timer
  ,unitParser:: Parser Exp1

unitParser = Unit <$ try (string "()")

ifParser = do
  string "if "
  e1 <- expParser
  whitespace
  e2 <- expParser
  whitespace
  e3 <- expParser
  return $ If1 e1 e2 e3

varParser = Var1 <$> idParser

appParser = parens (App1 <$> expParser <* space <*> sepEndBy expParser space)

opsParser = op2Parser "+ " Plus
            <|> op2Parser "- " Minus
            <|> op2Parser "* " Mult
            <|> op2Parser "%/ " Div
            <|> op2Parser "=" Eq
            <|> op2Parser ">= " Ge
            <|> op2Parser "> " Gt
            <|> op2Parser "<= " Le
            <|> op2Parser "< " Lt
            <|> op2Parser "%>> " ShiftR
            <|> op2Parser "%<< " ShiftL
            <|> op2Parser "binary-and " BAnd
            <|> op2Parser "binary-or " BOr

intParser = N1 <$> integer

boolParser = (\x -> B1 $ x == 't') <$ char '#' <*> (char 't' <|> char 'f')

lambdaParser = do
  try $ string "lambda ("
  args <- sepEndBy argParser whitespace
  string ") : "
  rt <- typeParser
  whitespace
  b <- expParser
  return $ Lam1 args b rt

letParser = Let1 <$ try (string "let (") <*> sepEndBy bindParser space
            <* char ')' <* whitespace <*> expParser
letrecParser = Letrec1 <$ try (string "letrec (") <*> sepEndBy bindParser space
               <* string ")" <* whitespace <*> expParser

grefParser = c1Parser "gbox " GRef1
gderefParser = c1Parser "gunbox " GDeRef1
grefsetParser = c2Parser "gbox-set! " GAssign1
mrefParser = c1Parser "mbox " MRef1
mderefParser = c1Parser "munbox " MDeRef1
mrefsetParser = c2Parser "mbox-set! " MAssign1
gvectParser = c2Parser "gvector " GVect1
gvectrefParser = c2Parser "gvector-ref " GVectRef1
gvectsetParser = c3Parser "gvector-set! " GVectSet1
mvectParser = c2Parser "mvector " GVect1
mvectrefParser = c2Parser "mvector-ref " MVectRef1
mvectsetParser = c3Parser "mvector-set! " MVectSet1

asParser = As1 <$ string ": " <*> expParser <* space <*> typeParser

beginParser = do
  try $ string "begin "
  es <- sepEndBy1 expParser space
  return $ Begin1 (init es) $ last es

repeatParser = do
  try $ string "repeat "
  char '('
  x <- idParser
  space
  start <- expParser
  space
  end <- expParser
  char ')'
  b <- expParser
  return $ Repeat1 x start end b

timer = (TimerStart1 <$ string "timer-start")
        <|> (TimerStop1 <$ string "timer-stop")
        <|> (TimerReport1 <$ string "timer-report")

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
funTyParser = parens (FunTy <$> many (typeParser <* space) <* string "-> " <*> typeParser)
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
schmlParser = id <$ many whitespace <*> expParser <* many whitespace <* eof

parser :: String -> Either ParseError L1
parser = parse schmlParser ""
  
-- main :: IO ()            
-- main = parseTest schmlParser "(letrec ([x : (Int Int -> Int) (lambda ([x : Int] [y : Int]) : Int (* x (: -2 Int)))] [y : Bool #f]) (let ([z : Int 5] [t : () ()]) (if (> 3 1) (x z) 0)))"
