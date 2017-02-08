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
   "mvector-ref","mvector-set!","box",
   "box-set!","unbox","vector","vector-ref",
   "vector-set!","repeat","time",
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

whitespace :: Parser ()
whitespace =
    choice [simpleWhitespace *> whitespace,lineComment *> whitespace,return ()]
  where
    lineComment = try (string ";;") *>
                  manyTill anyChar (void (char '\n') <|> eof)
    simpleWhitespace = void $ many1 (oneOf " \t\n")

integer :: Parser Integer
integer =  fmap read integer1

-- (<++>) :: Applicative f => f [a] -> f [a] -> f [a]
-- (<++>) a b = (++) <$> a <*> b

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

number,plus,minus,integer1 :: Parser String

number = many1 digit

plus = char '+' *> number

minus = char '-' <:> number

integer1 = plus <|> minus <|> number

opnParser :: Int -> String -> Operator -> Parser L1
opnParser n s op = do
  src <- getPosition
  try (string ('(':s))
  whitespace
  es <- count n (expParser <* whitespace)
  char ')'
  return $ Ann src $ Op op es

c1Parser :: String -> (L1 -> ExpF1 (Ann SourcePos ExpF1)) -> Parser L1
c1Parser s op = do
  src <- getPosition
  try (string ('(':s))
  e <- expParser
  char ')'
  return $ Ann src $ op e

c2Parser :: String -> (L1 -> L1 -> ExpF1 (Ann SourcePos ExpF1)) -> Parser L1
c2Parser s op = do
  src <- getPosition
  try (string ('(':s))
  e1 <- expParser
  whitespace
  e2 <- expParser
  char ')'
  return $ Ann src $ op e1 e2

c3Parser :: String -> (L1 -> L1 -> L1 -> ExpF1 (Ann SourcePos ExpF1)) -> Parser L1
c3Parser s op = do
  src <- getPosition
  try (string ('(':s))
  e1 <- expParser
  whitespace
  e2 <- expParser
  whitespace
  e3 <- expParser
  char ')'
  return $ Ann src $ op e1 e2 e3

specialChar :: Parser Char
specialChar = fstSpecChar -- <|> oneOf "!"

fstSpecChar :: Parser Char
fstSpecChar = oneOf "_#%*+-!^"
  
idParser :: Parser String
idParser = do
  name <- (:) <$> (letter <|> specialChar) <*> many (alphaNum <|> specialChar)
  if isReservedName name
    then unexpected ("reserved word " ++ show name)
    else return name

argParser :: Parser (Name,Type)
argParser =
  (,) <$ char '[' <*> idParser <* string " : " <*> typeParser <* char ']'
  <|> (\d -> (d,BlankTy)) <$> idParser

bindParser :: Parser (Bind Type L1)
bindParser = do
  c <- char '[' <|> char '('
  x <- idParser
  whitespace
  t <- option BlankTy (id <$ char ':' <* whitespace <*> typeParser <* whitespace)
  e <- expParser
  if c == '[' then char ']' else char ')'
  return (Bind x t e)

ifParser,varParser,appParser,opsParser,intParser,boolParser
  ,lambdaParser,letParser,letrecParser,refParser,derefParser
  ,refsetParser,grefParser,gderefParser,grefsetParser,mrefParser,
   mderefParser,mrefsetParser,vectParser,vectrefParser,
   vectsetParser,gvectParser,gvectrefParser,
   gvectsetParser,mvectParser,mvectrefParser,mvectsetParser,asParser,
   beginParser,repeatParser,unitParser,timeParser,topLevParser,
   floatParser,charParser :: Parser L1
                             
unitParser = Ann <$> getPosition <*> (P Unit <$ try (string "()"))

floatParser = do
  i <- option "" (string "#i")
  int <- integer1
  dec <- decimal
  ex  <- expon
  let s = i ++ int ++ dec ++ ex
      f = rd s
  annotate $ return $ P $ F f s
    where rd       = read :: String -> Double
          decimal  = option "" $ char '.' <:> number
          expon = option "" $ oneOf "eE" <:> integer1

ifParser = do
  src <- getPosition
  try (string "(if ")
  e1 <- expParser
  whitespace
  e2 <- expParser
  whitespace
  e3 <- expParser
  char ')'
  return $ Ann src $ If e1 e2 e3

varParser = annotate ((P . Var) <$> idParser)

appParser = do
  src <- getPosition
  char '('
  e <- expParser
  whitespace
  es <- sepEndBy expParser whitespace
  char ')'
  return $ Ann src $ App e es

op0Parser,op1Parser,op2Parser :: String -> Operator -> Parser L1
op0Parser = opnParser 0
op1Parser = opnParser 1
op2Parser = opnParser 2

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
            <|> op2Parser "fl+ " PlusF
            <|> op2Parser "fl- " MinusF
            <|> op2Parser "fl* " MultF
            <|> op2Parser "fl/ " DivF
            <|> op1Parser "flmodulo " ModuloF
            <|> op1Parser "flabs " AbsF
            <|> op2Parser "fl< " LtF
            <|> op2Parser "fl<= " LeF
            <|> op2Parser "fl= " EqF
            <|> op2Parser "fl> " GtF
            <|> op2Parser "fl>= " GeF
            <|> op2Parser "flmin " MinF
            <|> op2Parser "flmax " MaxF
            <|> op1Parser "flfloor " RoundF
            <|> op1Parser "flround " FloorF
            <|> op1Parser "flceiling " CeilingF
            <|> op1Parser "fltruncate " TruncateF
            <|> op1Parser "flsin " SinF
            <|> op1Parser "flcos " CosF
            <|> op1Parser "fltan " TanF
            <|> op1Parser "flasin " AsinF
            <|> op1Parser "flacos " AcosF
            <|> op1Parser "flatan " AtanF
            <|> op1Parser "fllog " LogF
            <|> op1Parser "flexp " ExpF
            <|> op1Parser "flsqrt " SqrtF
            <|> op2Parser "flexpt " ExptF
            <|> op1Parser "float->int " FloatToInt
            <|> op1Parser "int->float " IntToFloat
            <|> op1Parser "char->int " CharToInt
            <|> op0Parser "read-int" ReadInt
            <|> op0Parser "read-float" ReadFloat
            
            

intParser = annotate $ (P . N) <$> try integer

boolParser = annotate $ (\x -> (P . B) $ x == 't') <$ char '#' <*> (char 't' <|> char 'f')

charParser = annotate $ (P . C) <$ string "#\\" <*> (try (string "space") <|> try ((: []) <$> anyChar))

lambdaParser = do
  src <- getPosition
  try (string "(lambda (")
  args <- sepEndBy argParser whitespace
  char ')'
  whitespace
  rt <- option BlankTy (id <$ char ':' <* whitespace <*> typeParser <* whitespace)
  b <- expParser
  char ')'
  return $ Ann src $ Lam (map fst args) b $ ArrTy (map snd args) rt

letParser = do
  src <- getPosition
  try (string "(let")
  whitespace
  char '('
  binds <- sepEndBy bindParser whitespace
  char ')'
  whitespace
  e <- expParser
  char ')'
  return $ Ann src $ Let (Binds binds) e

letrecParser = do
  src <- getPosition
  try (string "(letrec")
  whitespace
  char '('
  binds <- sepEndBy bindParser whitespace
  char ')'
  whitespace
  e <- expParser
  char ')'
  return $ Ann src $ Letrec (Binds binds) e

timeParser = c1Parser "time " Time
refParser = c1Parser "box " Ref
derefParser = c1Parser "unbox " DeRef
refsetParser = c2Parser "box-set! " Assign
grefParser = c1Parser "gbox " GRef
gderefParser = c1Parser "gunbox " GDeRef
grefsetParser = c2Parser "gbox-set! " GAssign
mrefParser = c1Parser "mbox " MRef
mderefParser = c1Parser "munbox " MDeRef
mrefsetParser = c2Parser "mbox-set! " MAssign
vectParser = c2Parser "vector " Vect
vectrefParser = c2Parser "vector-ref " VectRef
vectsetParser = c3Parser "vector-set! " VectSet
gvectParser = c2Parser "gvector " GVect
gvectrefParser = c2Parser "gvector-ref " GVectRef
gvectsetParser = c3Parser "gvector-set! " GVectSet
mvectParser = c2Parser "mvector " MVect
mvectrefParser = c2Parser "mvector-ref " MVectRef
mvectsetParser = c3Parser "mvector-set! " MVectSet

asParser = do
  src <- getPosition
  try (string "(: ")
  e <- expParser
  space
  t <- typeParser
  char ')'
  return $ Ann src $ As e t

dconstParser :: Parser (Def Type L1)
dconstParser = do
  try $ do string "(define"
           whitespace
  x <- idParser
  whitespace
  t <- option BlankTy (string ": " *> typeParser <* whitespace)
  e <- expParser
  char ')'
  return $ DConst x t e

dlamParser :: Parser (Def Type L1)
dlamParser = do
  try $ do string "(define"
           whitespace
           char '('
  x <- idParser
  whitespace
  args <- sepEndBy argParser whitespace
  char ')'
  whitespace
  rt <- option BlankTy (id <$ char ':' <* whitespace <*> typeParser)
  whitespace
  b <- expParser
  whitespace
  char ')'
  return $ DLam x (map fst args) b $ ArrTy (map snd args) rt

beginParser = do
  src <- getPosition
  try (string "(begin")
  whitespace
  es <- sepEndBy1 expParser whitespace
  char ')'
  return $ Ann src $ Begin (init es) $ last es

repeatParser = do
  src <- getPosition
  try (string "(repeat")
  whitespace
  char '('
  x <- idParser
  whitespace
  start <- expParser
  whitespace
  end <- expParser
  char ')'
  whitespace
  char '('
  acci <- idParser
  whitespace
  acct <- option BlankTy (id <$ char ':' <* whitespace <*> typeParser <* whitespace)
  acce <- expParser
  char ')'
  whitespace
  b <- expParser
  char ')'
  return $ Ann src $ Repeat x acci start end b acce acct

topLevParser = do
  src <- getPosition
  ds <- sepEndBy (dlamParser <|> dconstParser) whitespace
  es <- sepEndBy expParser whitespace
  return $ Ann src $ TopLevelDefs (Defs ds) es

expParser :: Parser L1
expParser = try floatParser
            <|> intParser
            <|> try boolParser
            <|> try charParser
            <|> unitParser
            <|> try varParser
            <|> opsParser
            <|> ifParser
            <|> lambdaParser
            <|> refParser
            <|> derefParser
            <|> refsetParser
            <|> grefParser
            <|> gderefParser
            <|> grefsetParser
            <|> mrefParser
            <|> mderefParser
            <|> mrefsetParser
            <|> vectParser
            <|> vectrefParser
            <|> vectsetParser
            <|> gvectParser
            <|> gvectrefParser
            <|> gvectsetParser
            <|> mvectParser
            <|> mvectrefParser
            <|> mvectsetParser
            <|> timeParser
            <|> letrecParser
            <|> letParser
            <|> asParser
            <|> beginParser
            <|> repeatParser
            <|> try appParser

-- Type Parsers

refTyParser,vectTyParser,grefTyParser,mrefTyParser,gvectTyParser
  ,mvectTyParser,intTyParser,boolTyParser,dynTyParser,unitTyParser
  ,funTyParser,floatTyParser,charTyParser,tupleTyParser,typeParser
   :: Parser Type

charTyParser   = CharTy <$ try (string "Char")
intTyParser    = IntTy <$ try (string "Int")
floatTyParser  = FloatTy <$ try (string "Float")
boolTyParser   = BoolTy <$ try (string "Bool")
dynTyParser    = Dyn <$ try (string "Dyn")
unitTyParser   = UnitTy <$ try (string "()" <|> string "Unit")
funTyParser    = do
  char '('
  ts <- sepEndBy typeParser whitespace
  string "-> "
  rt <- typeParser
  char ')'
  return $ FunTy ts rt
  
tupleTyParser = do
  string "(Tuple "
  ts <- sepEndBy typeParser whitespace
  char ')'
  return $ TupleTy ts

refTyParser   = RefTy <$ try (string "(Ref ") <*> typeParser <* char ')'
vectTyParser  = VectTy <$ try (string "(Vect ") <*> typeParser <* char ')'
grefTyParser  = GRefTy <$ try (string "(GRef ") <*> typeParser <* char ')'
mrefTyParser  = MRefTy <$ try (string "(MRef ") <*> typeParser <* char ')'
gvectTyParser = GVectTy <$ try (string "(GVect ") <*> typeParser <* char ')'
mvectTyParser = MVectTy <$ try (string "(MVect ") <*> typeParser <* char ')'

typeParser = charTyParser
             <|> intTyParser
             <|> floatTyParser
             <|> boolTyParser
             <|> dynTyParser
             <|> unitTyParser
             <|> try funTyParser
             <|> refTyParser
             <|> vectTyParser
             <|> grefTyParser
             <|> mrefTyParser
             <|> gvectTyParser
             <|> mvectTyParser
             <|> tupleTyParser

schmlParser :: Parser L1
schmlParser = id <$ whitespace <*> topLevParser <* whitespace <* eof

parser :: String -> Either ParseError L1
parser = parse schmlParser ""
  
-- main :: IO ()            
-- main = parseTest schmlParser "(letrec ([x : (Int Int -> Int) (lambda ([x : Int] [y : Int]) : Int (* x (: -2 Int)))] [y : Bool #f]) (let ([z : Int 5] [t : () ()]) (if (> 3 1) (x z) 0)))"
