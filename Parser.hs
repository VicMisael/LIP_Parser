import Data.Char
-- Meu programa possui um codigo que pass as strings para tokens para facilitar o trabalho
-- Main
main = do
  let result = parseStrings ["program", "bar", ";", "begin", "while", "a", "+", "3", "<", "b", "do", "b", ":=", "b", "+", "1", ";", "while", "a", "+", "3", "<", "b", "do", "b", ":=", "b", "+", "1", ";", "while", "a", "+", "3", "<", "b", "do", "b", ":=", "b", "+", "1", "end", "end"]
  print result

-- Definindo tipos de dado
data Token
  = Program
  | Semicolon
  | End
  | IntegerToken Integer
  | Id String
  | Begin
  | Assign
  | OpeningBracket
  | ClosingBracket
  | If
  | Then
  | Else
  | While
  | Do
  | Write
  | Read
  | EOP String
  | COP String
  | TOP String
  deriving (Show)

-- Definindo dados da árvore
newtype AstInteger = AstInteger Integer deriving (Show)

newtype AstId = AstId String deriving (Show)

data AstCop
  = Equal
  | Unequal
  | BiggerThan
  | SmallerThan
  | BiggerOrEqualThan
  | SmallerOrEqualThan
  deriving (Show)

data AstOp = Plus | Minus | Multiply | Divide deriving (Show)

data AstExpr = Identifier AstId | AstInt AstInteger | Op AstOp AstExpr AstExpr | AstComp AstCop AstExpr AstExpr deriving (Show)

data AstStat = AstSemicolon AstStat AstStat | AstAssign AstId AstExpr | AstIf AstExpr AstStat AstStat | AstWhile AstExpr AstStat | AstRead AstId | AstWrite AstExpr deriving (Show)

data AstProg = Prog AstId AstStat deriving (Show)

-- Lista de strings para arvore
parseStrings :: [String] -> AstProg
parseStrings xs = let (tks, tree) = parserProg (parseToken xs) in tree

-- Parser
parserProg :: [Token] -> ([Token], AstProg)
parserProg xs = case head xs of
  Program ->
    let s2 = tail xs
     in let (tokens, idToken) = parserId s2
         in case head tokens of
              Semicolon ->
                let s4 = tail tokens
                 in let (tokens, statTree) = parseStatement s4
                     in case head tokens of
                          End -> (tail tokens, Prog idToken statTree)
                          _ -> error "missing end"
              _ -> error "missing semicolon "
  _ -> error "missing start with program"

-- Preciso do sequence pra esses todos
parseStatement :: [Token] -> ([Token], AstStat)
parseStatement ((Id str) : xs) =
  case head xs of
    Assign ->
      let (tks, expr) = parseExpr (tail xs)
       in (tks, AstAssign (AstId str) expr)
    _ -> error "Missing assign"
parseStatement (Begin : xs) = parseSequenceStat xs parseStatement isSEP
parseStatement (If : xs) =
  let (tks, parsedC) = parseComp xs
   in case head tks of
        Then ->
          let (tks2, x1) = parseStatement (tail tks)
           in case head tks2 of
                Else ->
                  let (tks3, x2) = parseStatement (tail tks2)
                   in (tks3, AstIf parsedC x1 x2)
                _ -> error "Expected else"
        _ -> error "Expected then"
parseStatement (While : xs) =
  let (tks, c) = parseComp xs
   in case head tks of
        Do ->
          let (tks2, x) = parseStatement (tail tks)
           in (tks2, AstWhile c x)
        _ -> error ("expected Do got " ++ show (head tks) ++ " at " ++ show (tks))
parseStatement (Read : xs) = let (tks, rIdentifier) = parserId xs in (tks, AstRead rIdentifier)
parseStatement (write : xs) = let (tks, expr) = parseExpr xs in (tks, AstWrite expr)
parseStatement [] = error "Error at parseStatement"

isSEP :: Token -> Bool
isSEP Semicolon = True
isSEP _ = False

isEOP :: Token -> Bool
isEOP (EOP _) = True
isEOP x = False

isTOP :: Token -> Bool
isTOP (TOP _) = True
isTOP _ = False

isCOP :: Token -> Bool
isCOP (COP _) = True
isCOP _ = False

parseComp :: [Token] -> ([Token], AstExpr)
parseComp xs = parseSequenceExpr xs parseExpr isCOP

parseExpr :: [Token] -> ([Token], AstExpr)
parseExpr xs = parseSequenceExpr xs parseTerm isEOP

parseTerm :: [Token] -> ([Token], AstExpr)
parseTerm xs = parseSequenceExpr xs parseFact isTOP

parseSequenceStat :: [Token] -> ([Token] -> ([Token], AstStat)) -> (Token -> Bool) -> ([Token], AstStat)
parseSequenceStat xs nonTerm sep =
  let (tks, statTree) = nonTerm xs
   in if sep (head tks)
        then
          let x = head tks
              (x2, statTree2) = parseSequenceStat (tail tks) nonTerm sep
           in case x of
                Semicolon -> (x2, AstSemicolon statTree statTree2)
                _ -> error "Broken operator"
        else (tks, statTree)

parseSequenceExpr :: [Token] -> ([Token] -> ([Token], AstExpr)) -> (Token -> Bool) -> ([Token], AstExpr)
parseSequenceExpr xs nonTerm sep =
  let (tks, statTree) = nonTerm xs
   in if sep (head tks)
        then
          ( let x = head tks
                (tks2, statTree2) = parseSequenceExpr (tail tks) nonTerm sep
             in case x of
                  (EOP "+") -> (tks2, Op Plus statTree statTree2)
                  (EOP "-") -> (tks2, Op Minus statTree statTree2)
                  (TOP "*") -> (tks2, Op Multiply statTree statTree2)
                  (TOP "/") -> (tks2, Op Divide statTree statTree2)
                  (COP "==") -> (tks2, AstComp Equal statTree statTree2)
                  (COP "!=") -> (tks2, AstComp Unequal statTree statTree2)
                  (COP ">") -> (tks2, AstComp BiggerThan statTree statTree2)
                  (COP "<") -> (tks2, AstComp SmallerThan statTree statTree2)
                  (COP "=<") -> (tks2, AstComp SmallerOrEqualThan statTree statTree2)
                  (COP ">=") -> (tks2, AstComp BiggerOrEqualThan statTree statTree2)
                  _ -> error "Broken operator"
          )
        else (tks, statTree)

parseFact :: [Token] -> ([Token], AstExpr)
parseFact xs = case head xs of
  (IntegerToken i) -> (tail xs, AstInt (AstInteger i))
  (Id str) -> (tail xs, Identifier (AstId str))
  OpeningBracket ->
    let s2 = tail xs
     in let (tks, s3) = parseExpr s2
         in case head tks of
              ClosingBracket -> (tail tks, s3)
              _ -> error ("expected ClosingBracket got " ++ show (head tks) ++ " at " ++ show (tks))
  _ -> error ("Error in Fact " ++ show (head xs) ++ "at" ++ show (xs))

parserIsAtom :: Token -> Bool
parserIsAtom (Id _) = True
parserIsAtom _ = False

parserIsIdent :: Token -> Bool
parserIsIdent OpeningBracket = False
parserIsIdent ClosingBracket = False
parserIsIdent t = parserIsAtom t

parserTokenIdToAstId :: Token -> AstId
parserTokenIdToAstId (Id str) = AstId str
parserTokenIdToAstId _ = error "not and Id Token"

parserId :: [Token] -> ([Token], AstId)
parserId xs =
  let tokenHead = head xs
   in case tokenHead of
        Id str -> (tail xs, AstId str)
        _ -> error "Is not an Id"

-- Processamento dos Tokens

isNumericString :: String -> Bool
isNumericString str = case dropWhile isDigit str of
  "" -> True
  _ -> False

tokenize :: String -> Token
tokenize "program" = Program
tokenize ";" = Semicolon
tokenize "end" = End
tokenize "begin" = Begin
tokenize ":=" = Assign
tokenize "(" = OpeningBracket
tokenize ")" = ClosingBracket
tokenize "if" = If
tokenize "then" = Then
tokenize "else" = Else
tokenize "while" = While
tokenize "do" = Do
tokenize "read" = Read
tokenize "write" = Write
tokenize "==" = COP "=="
tokenize "!=" = COP "!="
tokenize ">" = COP ">"
tokenize "<" = COP "<"
tokenize "=<" = COP "=<"
tokenize ">=" = COP ">="
tokenize "+" = EOP "+"
tokenize "-" = EOP "-"
tokenize "*" = TOP "*"
tokenize "/" = TOP "/"
tokenize x
  | isNumericString x =
    IntegerToken (read x :: Integer)
  | otherwise = Id x

parseToken :: [String] -> [Token]
parseToken xs = [tokenize x | x <- xs]
