module SemanticAnalyzer where
import AST
import ErrorMonad
import Parser


formatM :: Show a => M a -> String
formatM (MS (s, a)) = "Messages:\n" ++ s ++ "\nAST: " ++ show a

emitError :: Show a => String -> a -> M ()
emitError msg line = 
  MS ("ERROR -> " ++ msg ++ " in expression:\n\t" ++ show line ++ "\n\n", ())

emitWarning :: Show a => String -> a -> M ()
emitWarning msg line =
  MS ("WARNING -> " ++ msg ++ " in expression\n\t" ++ show line++ "\n\n", ())

lookupVar :: Id -> [Var] -> Maybe Tipo
lookupVar id [] = Nothing
lookupVar id ((idd :#: (varType, _)):vs)
  | id == idd = Just varType
  | otherwise = lookupVar id vs

exprTypeCheck :: [Var] -> Expr -> M (Tipo, Expr)
exprTypeCheck _ (Const (CInt n))     = pure (TInt, Const (CInt n))
exprTypeCheck _ (Const (CDouble n))  = pure (TDouble, Const (CDouble n))
exprTypeCheck _ (Lit s)  = pure (TString, Lit s)

exprTypeCheck vars (IdVar id) = 
  case lookupVar id vars of
    Just varType -> pure(varType, IdVar id)
    Nothing -> do 
      emitError ("varible " ++ show id ++ " not declared") (IdVar id)
      return (TVoid, IdVar id)

exprTypeCheck vars (Neg e)  = do
  (t, new_e) <- exprTypeCheck vars e
  if (t == TString)
    then do
      emitError "Cannot make a string negative" e
      return (TVoid, e)
  else pure(t, new_e)

exprTypeCheck vars (IntDouble e) = do
  (t, new_e) <- exprTypeCheck vars e
  if t == TInt 
    then pure (TDouble, IntDouble new_e)
    else do
      emitError "Cannot convert non-integer to double" e
      return (t, new_e)

exprTypeCheck vars (DoubleInt e) = do
  (t, new_e) <- exprTypeCheck vars e
  if t == TDouble 
    then pure (TInt, DoubleInt new_e)
    else do
      emitError "Cannot convert non-double to integer" e
      return (t, new_e)

exprTypeCheck vars (Add e1 e2) = checkExpr vars e1 e2 Add
exprTypeCheck vars (Sub e1 e2) = checkExpr vars e1 e2 Sub
exprTypeCheck vars (Mul e1 e2) = checkExpr vars e1 e2 Mul
exprTypeCheck vars (Div e1 e2) = checkExpr vars e1 e2 Div
exprTypeCheck _ expr = do 
  emitError "Type checking not implemented for this expression" expr
  return (TVoid, expr)

checkExpr :: [Var] -> Expr -> Expr -> (Expr -> Expr -> Expr) -> M (Tipo, Expr)
checkExpr vars e1 e2 operation = do
  (t1, new_e1) <- exprTypeCheck vars e1
  (t2, new_e2) <- exprTypeCheck vars e2
  let op = operation e1 e2
  let new_op = operation new_e1 new_e2
  case (t1, t2) of
    (TString, _) -> do 
      emitError ("Cannot operate string with " ++ show t2) op
      return (TString, new_op)
    (_, TString) -> do 
      emitError ("Cannot operate " ++ show t1 ++ " with string") op
      return (TString, new_op)
    (TInt, TInt) -> pure (TInt, new_op)
    (TDouble, TDouble) -> pure (TDouble, new_op)
    (TInt, TDouble) -> do 
      emitWarning "Coercing int to double" op
      return (TDouble, operation (IntDouble new_e1) new_e2)
    (TDouble, TInt) -> do
      emitWarning "Coercing int to double" op
      return (TDouble, operation new_e1 (IntDouble new_e2))
    _ -> do 
      emitError "Unsupported types to operate" op
      return (TVoid, new_op)

exprRTypeCheck :: [Var] -> ExprR -> M ExprR
exprRTypeCheck vars (Req e1 e2) = checkExprR vars e1 e2 Req
exprRTypeCheck vars (Rdif e1 e2) = checkExprR vars e1 e2 Rdif
exprRTypeCheck vars (Rlt e1 e2) = checkExprR vars e1 e2 Rlt
exprRTypeCheck vars (Rgt e1 e2) = checkExprR vars e1 e2 Rgt
exprRTypeCheck vars (Rle e1 e2) = checkExprR vars e1 e2 Rle
exprRTypeCheck vars (Rge e1 e2) = checkExprR vars e1 e2 Rge

checkExprR :: [Var] -> Expr -> Expr -> (Expr -> Expr -> ExprR) -> M ExprR
checkExprR vars e1 e2 operation = do
  (t1, new_e1) <- exprTypeCheck vars e1
  (t2, new_e2) <- exprTypeCheck vars e2
  let op = operation e1 e2
  let new_op = operation new_e1 new_e2
  case (t1, t2) of
    (TString, TString) -> pure new_op
    (TString, _) -> do 
      emitError ("Cannot compare string with " ++ show t2) op
      return new_op
    (_, TString) -> do
      emitError ("Cannot compare " ++ show t1 ++ " with string") op
      return new_op
    (TInt, TInt) -> pure new_op
    (TDouble, TDouble) -> pure new_op
    (TInt, TDouble) -> do 
      emitWarning "Coercing int to double" op
      return (operation (IntDouble new_e1) new_e2)
    (TDouble, TInt) -> do 
      emitWarning "Coercing int to double" op
      return (operation new_e1 (IntDouble new_e2))
    _ -> do 
      emitError "Unsupported types to compare" op
      return new_op
  
exprLTypeCheck :: [Var] -> ExprL -> M ExprL
exprLTypeCheck vars (And e1 e2) = checkExprL vars e1 e2 And
exprLTypeCheck vars (Or e1 e2)  = checkExprL vars e1 e2 Or
exprLTypeCheck vars (Not e1)    = exprLTypeCheck vars e1
exprLTypeCheck vars (Rel e1)    = pure Rel <*> exprRTypeCheck vars e1

checkExprL :: [Var] -> ExprL -> ExprL -> (ExprL -> ExprL -> b) -> M b
checkExprL vars e1 e2 operation = do
  new_e1 <- exprLTypeCheck vars e1
  new_e2 <- exprLTypeCheck vars e2
  let new_op = operation new_e1 new_e2
  pure new_op

checkBlock :: Maybe Funcao -> [Var] -> Bloco -> M Bloco
checkBlock function vars (cmd : block) = do
  new_cmd <- checkCommand function vars cmd
  new_block <- checkBlock function vars block
  pure (new_cmd : new_block)

checkBlock function vars [] = pure []

checkCommand :: Maybe Funcao -> [Var] -> Comando -> M Comando

checkCommand function vars (If exprL b1 b2) = do
  new_e <- exprLTypeCheck vars exprL
  new_b1 <- checkBlock function vars b1
  new_b2 <- checkBlock function vars b2
  pure(If new_e new_b1 new_b2)

checkCommand function vars (While exprL b) = do
  new_e <- exprLTypeCheck vars exprL
  new_b <- checkBlock function vars b
  pure(While new_e new_b)

checkCommand _ vars (Imp expr) = do
  (_, new_e) <- exprTypeCheck vars expr
  pure(Imp new_e)


checkCommand _ vars (Atrib var expr) = do
  (eType, new_expr) <- exprTypeCheck vars expr
  let cmd = Atrib var expr
  let new_cmd = Atrib var new_expr
  case lookupVar var vars of
    Just varType -> do
      case (varType, eType) of
        (TString, TString) -> pure new_cmd
        (TString, _) -> do
          emitError ("Cannot assign " ++ show eType ++ " to string") cmd
          return new_cmd
        (_, TString) -> do
          emitError ("Cannot assign string to " ++ show eType) cmd
          return new_cmd
        (TInt, TInt) -> pure new_cmd
        (TDouble, TDouble) -> pure new_cmd
        (TInt, TDouble) -> do
          emitWarning "Coercing double to int" cmd
          return (Atrib var (DoubleInt expr))
        (TDouble, TInt) -> do
          emitWarning "Coercing int to double" cmd
          return (Atrib var (IntDouble expr))
        _ -> do
          emitError "Unsuported types to assign" cmd
          return new_cmd
    Nothing -> do
      emitError ("varible " ++ show var ++ " not declared") new_cmd
      return cmd

checkCommand _ vars (Leitura var) = do
  let cmd = Leitura var
  case lookupVar var vars of
    Just varType -> pure cmd
    Nothing -> do
      emitError ("varible " ++ show var ++ " not declared") cmd
      return cmd

checkCommand function vars (Ret maybeExpr) = do
  let returnType = case function of
        Just (id :->: (_, retType)) -> retType
        Nothing -> TVoid
  let cmd = Ret maybeExpr
  case maybeExpr of
    Just e -> do
      (eType, new_e) <- exprTypeCheck vars e
      case (eType, returnType) of
        (TString, TString) -> pure (Ret (Just new_e))
        (TString, _) -> do
          emitError (show eType ++ " does not match expected return type " ++ show returnType) cmd
          return (Ret (Just new_e))
        (_, TString) -> do
          emitError (show eType ++ " does not match expected return type " ++ show returnType) cmd
          return (Ret (Just new_e))
        (TInt, TInt) -> pure (Ret (Just new_e))
        (TDouble, TDouble) -> pure (Ret (Just new_e))
        (TInt, TDouble) -> do
          emitWarning "Coercing double to int" cmd
          return (Ret (Just (DoubleInt new_e)))
        (TDouble, TInt) -> do
          emitWarning "Coercing int to double" cmd
          return (Ret (Just (IntDouble new_e)))
        _ -> do
          emitError "Unsuported types to assign" cmd
          return (Ret (Just new_e))
    Nothing ->
      case returnType of
        TVoid -> pure cmd
        _ -> do
          emitError "Non-void function expects a return expression" cmd
          pure cmd

checkFunction :: Funcao -> (Id, [Var], Bloco) -> M (Funcao, (Id, [Var], Bloco))
checkFunction function (id, vars, block) = do
  new_block <- checkBlock (Just function) vars block
  pure (function, (id, vars, new_block))

checkFunctionList :: [Funcao] -> [(Id, [Var], Bloco)] -> M ([Funcao], [(Id, [Var], Bloco)])
checkFunctionList [] [] = pure ([], [])
checkFunctionList (function : functionTail) ((id, vars, block) : rest) = do
  (new_function, new_functionBody) <- checkFunction function (id, vars, block)
  (tail_functionList, tail_functionBodyList) <- checkFunctionList functionTail rest
  pure (new_function : tail_functionList, new_functionBody : tail_functionBodyList)

checkProgram :: Programa -> M Programa
checkProgram (Prog functionList functionBodyList mainBlockVars mainBlock) = do
  (new_functionList, new_functionBodyList) <- checkFunctionList functionList functionBodyList
  new_mainBlock <- checkBlock Nothing mainBlockVars mainBlock
  pure (Prog new_functionList new_functionBodyList mainBlockVars new_mainBlock)
  

semantics :: IO ()
semantics = do
  writeFile "out" ""
  -- let expr1 = Rge (IdVar "x") (Div (Const (CInt 5)) (Const (CDouble 10.5)))
  -- let expr2 = And (Rel (Rge (Const (CInt 3)) (Const (CInt 1)))) (Rel (Rdif (Const (CInt 5)) (Const (CDouble 0.5))))
  -- let expr3 = [If (Rel (Rgt (IdVar "f") (Const (CInt 2)))) [Atrib "f" (Add (IdVar "f") (Const (CInt 1)))] [Atrib "f" (Add (IdVar "f") (Const (CInt 2)))]]
  -- let expr4 = [Leitura "f",If (Rel (Rgt (IdVar "f") (Const (CInt 2)))) [Atrib "f" (Add (IdVar "f") (Const (CInt 1)))] [Atrib "f" (Add (IdVar "f") (Const (CInt 2)))]]
  -- appendFile "out" (formatM $ exprLTypeCheck ["x" :#: (TDouble, 0)] expr2)
  -- appendFile "out" "\n\n\n\n"
  -- appendFile "out" (formatM $ exprRTypeCheck ["x" :#: (TInt, 0)] expr1)
  -- appendFile "out" "\n\n\n\n"
  -- appendFile "out" (formatM $ checkBlock Nothing ["x" :#: (TInt, 0)] expr3)
  -- appendFile "out" "\n\n\n\n"
  -- appendFile "out" (formatM $ checkBlock Nothing ["f" :#: (TInt, 0)] expr4)
  code <- readFile "AST.txt" 
  appendFile "out" (formatM $ checkProgram $ read code)