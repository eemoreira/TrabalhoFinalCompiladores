module SemanticAnalyzer where
import AST
import ErrorMonad
import Parser

errorM :: [Char] -> M()
errorM s = MS("error: " ++ s ++ "\n", ())

warningM :: [Char] -> M()
warningM s = MS("warning: " ++ s, ())

verificaExpr :: Expr -> M (Tipo, Expr)

verificaExpr (Const (CInt n)) = pure (TInt, Const (CInt n))
verificaExpr (Const (CDouble d)) = pure (TDouble, Const (CDouble d))
verificaExpr (Const (CString s)) = pure (TString, Const (CString s))

verificaExpr (Add e1 e2) = do
  (t1, ans1) <- verificaExpr e1
  (t2, ans2) <- verificaExpr e2
  if t1 == TString || t2 == TString then do
    errorM "operacao '+' não é válida com strings" <*> pure (TString, Add ans1 ans2)
    return (TString, astMod)
  else if t1 == t2 then
    return (t1, astMod)
  else do
    warningM "coercao de tipo: int para double"
    -- Realiza a coerção para TDouble
    let coercedE1 = if t1 == TInt then IntDouble ast1 else ast1
    let coercedE2 = if t2 == TInt then IntDouble ast2 else ast2
    return (TDouble, Add coercedE1 coercedE2)

test :: IO()
test = do
  let expr1 = Add (Const (CDouble 3.0)) (Add (Const (CInt 1)) (Const (CDouble 2.0)))
  let expr2 = Add (Const (CString "a")) (Const (CString "b"))
  -- let expr3 = Mul (Const (CInt 3)) (Const (CInt 4))
  print $ verificaExpr expr1  -- Esperado: Warning + AST modificada
  print $ verificaExpr expr2  -- Esperado: Erro
  -- print $ verificaExpr expr3  -- Esperado: AST sem modificações



