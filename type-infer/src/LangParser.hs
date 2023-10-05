module LangParser (parseSexp) where

import Data.Char
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Prim
import Text.Parsec.Combinator
-- import Control.Applicative ((<|>), many)

import LangCore as LC

parseSexp = parse parseExpr ""

parseExpr :: Parser Expr
parseExpr = try parseIdentifier <|>
            try parseNumber  <|> 
            try parseBool <|>
            try parseIf <|>
            try parseLet <|>
            try parseLambda <|>
            try parseBPrim <|>
            try parseUPrim <|>
            try parseApp

parseToken :: String -> Parser String
parseToken tok = try $ spaces >> string tok

parseIdentifier :: Parser Expr
parseIdentifier = do 
        spaces
        fc <- firstChar
        rest <- many nonFirstChar
        return $ Var (fc:rest)
    where firstChar = letter <|> char '_'
          nonFirstChar = alphaNum <|> char '_'


parseBool :: Parser Expr
parseBool = do
    tok <- choice [parseToken "#t", parseToken "#f"]
    return $ if tok == "#t" then Boolean True else Boolean False

parseNumber :: Parser Expr
parseNumber = do 
    spaces
    n <- many1 digit
    return $ Number $ read n


parseIf :: Parser Expr
parseIf = do 
    parseToken "("
    parseToken "if"
    pred <- parseExpr
    thn <- parseExpr
    els <- parseExpr
    parseToken ")"
    return $ If pred thn els
    
parseLet :: Parser Expr
parseLet = do
    parseToken "("
    parseToken "let"
    parseToken "("
    binds <- many bind
    parseToken ")"
    body <- parseExpr
    parseToken ")"
    return $ Let binds body
        where bind = do
                        parseToken "("
                        Var v <- parseIdentifier
                        expr <- parseExpr
                        parseToken ")"
                        return (v, expr)

parseLambda :: Parser Expr
parseLambda = do 
    parseToken "("
    parseToken "lambda"
    parseToken "("
    ps <- many $ do { Var v <- parseIdentifier; return v }
    parseToken ")"
    body <- parseExpr
    parseToken ")"
    return $ Lambda ps body


parseApp :: Parser Expr
parseApp = do
    parseToken "("
    func <- parseExpr
    args <- many parseExpr
    parseToken ")"
    return $ App func args


parseBPrim :: Parser Expr
parseBPrim = do
    parseToken "("
    op <- choice $ map parseToken ["+","-","*","/","mod","=","<",">","<=",">=","and","or"]
    e1 <- parseExpr
    e2 <- parseExpr
    parseToken ")"
    return $ case op of 
               "+" -> BPrim Add e1 e2
               "-" -> BPrim Sub e1 e2
               "*" -> BPrim Mul e1 e2
               "/" -> BPrim Div e1 e2
               "mod" -> BPrim Mod e1 e2
               "=" -> BPrim Equal e1 e2
               "<" -> BPrim LC.LT e1 e2
               ">" -> BPrim LC.GT e1 e2
               "<=" -> BPrim LE e1 e2
               ">=" -> BPrim GE e1 e2
               "and" -> BPrim And e1 e2
               "or" -> BPrim Or e1 e2

parseUPrim :: Parser Expr
parseUPrim = do
    parseToken "("
    spaces
    op <- choice [string "not"]
    e <- parseExpr
    parseToken ")"
    return $ case op of
                "not" -> UPrim Not e
