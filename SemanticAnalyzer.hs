module SemanticAnalyzer where
import AST
import ErrorMonad
import Parser

emitError :: Show a => String -> a -> M (Tipo, a)
emitError msg expr = 
  MS ("ERROR -> " ++ msg ++ " in expression:\n\t" ++ show expr ++ "\n\n", (TVoid, expr))

emitWarning :: Show a => String -> (Tipo, a) -> M (Tipo, a)
emitWarning msg expr =
  let (t, e) = expr in
  MS ("WARNING -> " ++ msg ++ " in expression\n\t" ++ show e ++ "\n\n", expr)

typeCheck :: Expr -> M (Tipo, Expr)
typeCheck (Const (CInt n))     = pure (TInt, Const (CInt n))
typeCheck (Const (CDouble n))  = pure (TDouble, Const (CDouble n))
typeCheck (Const (CString s))  = pure (TString, Const (CString s))

typeCheck (IntDouble e) = do
  (t, new_e) <- typeCheck e
  if t == TInt 
    then pure (TDouble, IntDouble new_e)
    else emitError "Cannot convert non-integer to double" new_e

typeCheck (DoubleInt e) = do
  (t, new_e) <- typeCheck e
  if t == TDouble 
    then pure (TInt, DoubleInt new_e)
    else emitError "Cannot convert non-double to integer" new_e

typeCheck (Add e1 e2) = checkExpr e1 e2 Add
typeCheck (Sub e1 e2) = checkExpr e1 e2 Sub
typeCheck (Mul e1 e2) = checkExpr e1 e2 Mul
typeCheck (Div e1 e2) = checkExpr e1 e2 Div
typeCheck expr = emitError "Type checking not implemented for this expression" expr

checkExpr :: Expr -> Expr -> (Expr -> Expr -> Expr) -> M (Tipo, Expr)
checkExpr e1 e2 operation = do
  (t1, new_e1) <- typeCheck e1
  (t2, new_e2) <- typeCheck e2
  let op = operation new_e1 new_e2
  case (t1, t2) of
    (TString, _) -> emitError ("Cannot operate string with " ++ show t2) op
    (_, TString) -> emitError ("Cannot operate " ++ show t1 ++ " with string") op
    (TInt, TInt) -> pure (TInt, op)
    (TDouble, TDouble) -> pure (TDouble, op)
    (TInt, TDouble) -> emitWarning "Coercing int to double" (TDouble, operation (IntDouble new_e1) new_e2)
    (TDouble, TInt) -> emitWarning "Coercing int to double" (TDouble, operation new_e1 (IntDouble new_e2))
    _ -> emitError "Unsupported types for this operation" op

semantics :: IO ()
semantics = do
  let expr1 = Mul (Const (CString "Opa")) (Div (Const (CInt 5)) (Const (CDouble 10.5)))
  writeFile "out" (formatM $ typeCheck expr1) -- Deve gerar um warning sobre coerção

