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
   "timer-report","timer-start","timer-stop",
   "read-int"]

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

c1Parser :: String -> (L1 -> ExpF1 (Ann SourcePos ExpF1)) -> Parser L1
c1Parser s op = do
  src <- getPosition
  try (string s)
  e <- expParser
  return $ Ann src $ op e

c2Parser :: String -> (L1 -> L1 -> ExpF1 (Ann SourcePos ExpF1)) -> Parser L1
c2Parser s op = do
  src <- getPosition
  try (string s)
  e1 <- expParser
  whitespace
  e2 <- expParser
  return $ Ann src $ op e1 e2

c3Parser :: String -> (L1 -> L1 -> L1 -> ExpF1 (Ann SourcePos ExpF1)) -> Parser L1
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

argParser :: Parser (Name,Type)
argParser =
  (,) <$ char '[' <*> idParser <* string " : " <*> typeParser <* char ']'
  <|> (\d -> (d,Dyn)) <$> idParser

bindParser :: Parser (Bind L1)
bindParser = do
  c <- char '[' <|> char '('
  x <- idParser
  whitespace
  char ':'
  whitespace
  t <- typeParser
  whitespace
  e <- expParser
  if c == '[' then char ']' else char ')'
  return (x,t,e)

ifParser,varParser,appParser,opsParser,intParser,boolParser
  ,lambdaParser,letParser,letrecParser,grefParser,gderefParser
  ,grefsetParser,mrefParser,mderefParser,mrefsetParser,gvectParser
  ,gvectrefParser,gvectsetParser,mvectParser,mvectrefParser
  ,mvectsetParser,asParser,beginParser,repeatParser,misc
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

opsParser = try (op2Parser "+ " Plus)
            <|> try (op2Parser "- " Minus)
            <|> try (op2Parser "* " Mult)
            <|> try (op2Parser "%/ " Div)
            <|> try (op2Parser "= " Eq)
            <|> try (op2Parser ">= " Ge)
            <|> try (op2Parser "> " Gt)
            <|> try (op2Parser "<= " Le)
            <|> try (op2Parser "< " Lt)
            <|> try (op2Parser "%>> " ShiftR)
            <|> try (op2Parser "%<< " ShiftL)
            <|> try (op2Parser "binary-and " BAnd)
            <|> try (op2Parser "binary-or " BOr)

intParser = annotate $ N <$> try integer

boolParser = annotate $ (\x -> B $ x == 't') <$ char '#' <*> (char 't' <|> char 'f')

lambdaParser = do
  src <- getPosition
  try $ string "lambda ("
  args <- sepEndBy argParser whitespace
  char ')'
  whitespace
  rt <- optionMaybe (id <$ string ": " <*> typeParser <* whitespace)
  b <- expParser
  let t = ArrTy (map snd args) in
    return $ (Ann src . Lam (map fst args) b . maybe (t Dyn) t) rt

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
  binds <- sepEndBy bindParser whitespace
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
  try $ string "begin"
  whitespace
  es <- sepEndBy1 expParser whitespace
  return $ Ann src $ Begin (init es) $ last es

repeatParser = do
  src <- getPosition
  try $ string "repeat"
  whitespace
  char '('
  x <- idParser
  whitespace
  start <- expParser
  whitespace
  end <- expParser
  char ')'
  whitespace
  b <- expParser
  return $ Ann src $ Repeat x start end b

misc = annotate (TimerStart <$ try (string "(timer-start)"))
        <|> annotate (TimerStop <$ try (string "(timer-stop)"))
        <|> annotate (TimerReport <$ try (string "(timer-report)"))
        <|> annotate (ReadInt <$ try (string "(read-int)"))

expParser :: Parser L1
expParser = intParser
            <|> try boolParser
            <|> try unitParser
            <|> try (parens ifParser)
            <|> try varParser
            <|> try (parens lambdaParser)
            <|> try (parens grefParser)
            <|> try (parens gderefParser)
            <|> try (parens grefsetParser)
            <|> try (parens mrefParser)
            <|> try (parens mderefParser)
            <|> try (parens mrefsetParser)
            <|> try (parens gvectParser)
            <|> try (parens gvectrefParser)
            <|> try (parens gvectsetParser)
            <|> try (parens mvectParser)
            <|> try (parens mvectrefParser)
            <|> try (parens mvectsetParser)
            <|> try (parens letParser)
            <|> try (parens letrecParser)
            <|> try (parens asParser)
            <|> try (parens beginParser)
            <|> try (parens repeatParser)
            <|> try (parens opsParser)
            <|> try misc
            <|> try appParser

-- Type Parsers

grefTyParser,mrefTyParser,gvectTyParser,mvectTyParser,intTyParser
  ,boolTyParser,dynTyParser,unitTyParser,funTyParser
  ,typeParser :: Parser Type

intTyParser = IntTy <$ try (string "Int")
boolTyParser = BoolTy <$ try (string "Bool")
dynTyParser = Dyn <$ try (string "Dyn")
unitTyParser = UnitTy <$ try (string "()")
funTyParser = do
  char '('
  ts <- sepEndBy typeParser whitespace
  string "-> "
  rt <- typeParser
  char ')'
  return $ FunTy ts rt
  
grefTyParser = parens (GRefTy <$ try (string "GRef ") <*> typeParser)
mrefTyParser = parens (MRefTy <$ try (string "MRef ") <*> typeParser)
gvectTyParser = parens (GVectTy <$ try (string "GVect ") <*> typeParser)
mvectTyParser = parens (MVectTy <$ try (string "MVect ") <*> typeParser)

typeParser = intTyParser
             <|> boolTyParser
             <|> dynTyParser
             <|> unitTyParser
             <|> try funTyParser
             <|> try grefTyParser
             <|> try mrefTyParser
             <|> try gvectTyParser
             <|> try mvectTyParser

schmlParser :: Parser L1
schmlParser = id <$ whitespace <*> expParser <* whitespace <* eof

parser :: String -> Either ParseError L1
parser = parse schmlParser ""
  
-- main :: IO ()            
-- main = parseTest schmlParser "(letrec ([x : (Int Int -> Int) (lambda ([x : Int] [y : Int]) : Int (* x (: -2 Int)))] [y : Bool #f]) (let ([z : Int 5] [t : () ()]) (if (> 3 1) (x z) 0)))"
