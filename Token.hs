module Token where

data Token
  --tipos
  = TInt
  | TDouble
  | TString
  | TVoid
  -- relacoes
  | Eq
  | Diff
  | Lt
  | Gt
  | Le
  | Ge
  -- operacoes aritimeticas
  | Add
  | Sub
  | Mul 
  | Div 
  -- operacoes booleanas
  | And 
  | Or 
  | Not 
  -- numeros e strings
  | CDouble Double
  | CInt Int
  | CString String
  -- atribuicao
  | Assign
  -- identificador
  | Id String
  -- keywords
  | If
  | Else
  | While
  | Read 
  | Print 
  | Return
  -- adicionais
  | LPAR 
  | RPAR 
  | LBRACE
  | RBRACE
  | Comma
  | EndCommand
  deriving (Show, Eq)