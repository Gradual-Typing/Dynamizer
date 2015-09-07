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

annotate :: Monad m =>
            ParsecT s u m (e (Ann SourcePos e))
            -> ParsecT s u m (Ann SourcePos e)
annotate a = Ann <$> getPosition <*> a

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

op2Parser :: String -> Operator -> Parser L1
op2Parser s op = do
  src <- getPosition
  try $ string s
  e1 <- expParser
  whitespace
  e2 <- expParser
  return $ Ann src $ Op op [e1, e2]

c1Parser :: String -> (L1 -> ExpF (Ann SourcePos ExpF)) -> Parser L1
c1Parser s op = do
  src <- getPosition
  try (string s)
  e <- expParser
  return $ Ann src $ op e

c2Parser :: String -> (L1 -> L1 -> ExpF (Ann SourcePos ExpF)) -> Parser L1
c2Parser s op = do
  src <- getPosition
  try (string s)
  e1 <- expParser
  whitespace
  e2 <- expParser
  return $ Ann src $ op e1 e2

c3Parser :: String -> (L1 -> L1 -> L1 -> ExpF (Ann SourcePos ExpF)) -> Parser L1
c3Parser s op = do
  src <- getPosition
  try (string s)
  e1 <- expParser
  whitespace
  e2 <- expParser
  whitespace
  e3 <- expParser
  return $ Ann src $ op e1 e2 e3
  
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

bindParser :: Parser (Bind L1)
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

utBindParser :: Parser (UBind L1)
utBindParser = do
  char '[' <|> char '('
  x <- idParser
  whitespace
  e <- expParser
  char ']' <|> char ')'
  return (x,e)

ifParser,varParser,appParser,opsParser,intParser,boolParser
  ,lambdaParser,letParser,letrecParser,grefParser,gderefParser
  ,grefsetParser,mrefParser,mderefParser,mrefsetParser,gvectParser
  ,gvectrefParser,gvectsetParser,mvectParser,mvectrefParser
  ,mvectsetParser,asParser,beginParser,repeatParser,timer
  ,unitParser:: Parser L1

unitParser = Ann <$> getPosition <*> (Unit <$ try (string "()"))

ifParser = do
  src <- getPosition
  string "if "
  e1 <- expParser
  whitespace
  e2 <- expParser
  whitespace
  e3 <- expParser
  return $ Ann src $ If e1 e2 e3

varParser = annotate $ Var <$> idParser

appParser = do
  src <- getPosition
  char '('
  e <- expParser
  whitespace
  es <- sepEndBy expParser whitespace
  char ')'
  return $ Ann src $ App e es

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

intParser = annotate $ N <$> try integer

boolParser = annotate $ (\x -> B $ x == 't') <$ char '#' <*> (char 't' <|> char 'f')

lambdaParser = do
  src <- getPosition
  try $ string "lambda ("
  args <- sepEndBy argParser whitespace
  string ") : "
  rt <- typeParser
  whitespace
  b <- expParser
  return $ Ann src $ Lam args b rt

letParser = do
  src <- getPosition
  try (string "let (")
  binds <- sepEndBy bindParser whitespace
  char ')'
  whitespace
  e <- expParser
  return $ Ann src $ Let binds e

letrecParser = do
  src <- getPosition
  try (string "letrec (")
  binds <- sepEndBy utBindParser whitespace
  char ')'
  whitespace
  e <- expParser
  return $ Ann src $ Letrec binds e

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

asParser = do
  src <- getPosition
  string ": "
  e <- expParser
  space
  t <- typeParser
  return $ Ann src $ As e t

beginParser = do
  src <- getPosition
  try $ string "begin "
  es <- sepEndBy1 expParser whitespace
  return $ Ann src $ Begin (init es) $ last es

repeatParser = do
  src <- getPosition
  try $ string "repeat "
  char '('
  x <- idParser
  whitespace
  start <- expParser
  whitespace
  end <- expParser
  char ')'
  b <- expParser
  return $ Ann src $ Repeat x start end b

timer = (annotate $ TimerStart <$ string "timer-start")
        <|> (annotate $ TimerStop <$ string "timer-stop")
        <|> (annotate $ TimerReport <$ string "timer-report")

expParser :: Parser L1
expParser = intParser
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
            <|> try appParser
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
