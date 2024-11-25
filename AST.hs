module AST where
type Id = String

data Tipo = TDouble | TInt | TString | TVoid deriving (Show, Eq, Read)

data TCons = CDouble Double | CInt Int | CString String deriving (Show, Eq, Read)

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  | Const TCons
  | IdVar String
  | Chamada Id [Expr]
  | Lit String
  | IntDouble Expr
  | DoubleInt Expr
  deriving (Show, Read)

data ExprR
  = Req Expr Expr
  | Rdif Expr Expr
  | Rlt Expr Expr
  | Rgt Expr Expr
  | Rle Expr Expr
  | Rge Expr Expr
  deriving (Show, Read)

data ExprL = And ExprL ExprL | Or ExprL ExprL | Not ExprL | Rel ExprR
  deriving (Show, Read)

data Var = Id :#: (Tipo, Int) deriving (Show, Read)

data Funcao = Id :->: ([Var], Tipo) deriving (Show, Read)

data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco deriving (Show, Read)

type Bloco = [Comando]

data Comando
  = If ExprL Bloco Bloco
  | While ExprL Bloco
  | Atrib Id Expr
  | Leitura Id
  | Imp Expr
  | Ret (Maybe Expr)
  | Proc Id [Expr]
  deriving (Show, Read)


concatFunction :: (Funcao, (Id, [Var], Bloco)) -> ([Funcao], [(Id, [Var], Bloco)]) -> ([Funcao], [(Id, [Var], Bloco)])

concatFunction (function, (id, vars, block)) ([], []) = 
  ([function], [(id, vars, block)])

concatFunction (function, (id, vars, block)) (ansF, ansL) = 
  (function : ansF, (id, vars, block) : ansL)

  
createFunction :: Tipo -> Id -> [Var] -> [Var] -> Bloco -> (Funcao, (Id, [Var], Bloco))
createFunction tipo id parameterVars blockVars blockcommands =
  (fun, (id, parameterVars ++ blockVars, blockcommands))
    where fun = id :->: (parameterVars, tipo)