module SemanticAnalyzer where
import AST
import ErrorMonad
import Parser
import GHC.Exts.Heap (GenClosure(fun))


formatM :: Show a => M a -> String
formatM (MS (s, a)) = "Messages:\n" ++ s ++ "\nAST: " ++ show a

emitError :: Show a => String -> a -> M ()
emitError msg line = 
  MS ("ERROR -> " ++ msg ++ " in:\n\t" ++ show line ++ "\n\n", ())

emitWarning :: Show a => String -> a -> M ()
emitWarning msg line =
  MS ("WARNING -> " ++ msg ++ " in:\n\t" ++ show line++ "\n\n", ())

lookupVar :: Id -> [Var] -> Maybe Tipo
lookupVar id [] = Nothing
lookupVar id ((idd :#: (varType, _)) : xs)
  | id == idd = Just varType
  | otherwise = lookupVar id xs

lookupFunction :: Id -> [Funcao] -> Maybe Funcao
lookupFunction id [] = Nothing
lookupFunction id (fun@(idd :->: (_, _)) : xs)
  | id == idd = Just fun
  | otherwise = lookupFunction id xs

exprTypeCheck :: [Funcao] -> [Var] -> Expr -> M (Tipo, Expr)
exprTypeCheck functionList _ (Const (CInt n))     = pure (TInt, Const (CInt n))
exprTypeCheck functionList _ (Const (CDouble n))  = pure (TDouble, Const (CDouble n))
exprTypeCheck functionList _ (Lit s)  = pure (TString, Lit s)

exprTypeCheck functionList vars (IdVar id) = 
  case lookupVar id vars of
    Just varType -> pure(varType, IdVar id)
    Nothing -> do 
      emitError ("variable " ++ show id ++ " not declared") (IdVar id)
      return (TVoid, IdVar id)

exprTypeCheck functionList vars (Neg e)  = do
  (t, new_e) <- exprTypeCheck functionList vars e
  if (t == TString)
    then do
      emitError "Cannot make a string negative" e
      return (TVoid, e)
  else pure(t, new_e)

exprTypeCheck functionList vars (IntDouble e) = do
  (t, new_e) <- exprTypeCheck functionList vars e
  if t == TInt 
    then pure (TDouble, IntDouble new_e)
    else do
      emitError "Cannot convert non-integer to double" e
      return (t, new_e)

exprTypeCheck functionList vars (DoubleInt e) = do
  (t, new_e) <- exprTypeCheck functionList vars e
  if t == TDouble 
    then pure (TInt, DoubleInt new_e)
    else do
      emitError "Cannot convert non-double to integer" e
      return (t, new_e)

exprTypeCheck functionList vars (Add e1 e2) = checkExpr functionList vars e1 e2 Add
exprTypeCheck functionList vars (Sub e1 e2) = checkExpr functionList vars e1 e2 Sub
exprTypeCheck functionList vars (Mul e1 e2) = checkExpr functionList vars e1 e2 Mul
exprTypeCheck functionList vars (Div e1 e2) = checkExpr functionList vars e1 e2 Div
exprTypeCheck functionList vars (Chamada id exprList) = 
  case lookupFunction id functionList of
    Just function -> do
      let (_ :->: (_, returnType)) = function
      new_exprList <- checkFunctionCall functionList vars function function exprList
      return (returnType, Chamada id new_exprList)
    Nothing -> do
      emitError ("Function" ++ show id ++ " is not declared") (Chamada id exprList)
      return (TVoid, Chamada id exprList)

exprTypeCheck _ _ expr = do 
  emitError "Type checking not implemented for this expression" expr
  return (TVoid, expr)

checkExpr :: [Funcao] -> [Var] -> Expr -> Expr -> (Expr -> Expr -> Expr) -> M (Tipo, Expr)
checkExpr functionList vars e1 e2 operation = do
  (t1, new_e1) <- exprTypeCheck functionList vars e1
  (t2, new_e2) <- exprTypeCheck functionList vars e2
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

exprRTypeCheck :: [Funcao] -> [Var] -> ExprR -> M ExprR
exprRTypeCheck functionList vars (Req e1 e2)  = checkExprR functionList vars e1 e2 Req
exprRTypeCheck functionList vars (Rdif e1 e2) = checkExprR functionList vars e1 e2 Rdif
exprRTypeCheck functionList vars (Rlt e1 e2)  = checkExprR functionList vars e1 e2 Rlt
exprRTypeCheck functionList vars (Rgt e1 e2)  = checkExprR functionList vars e1 e2 Rgt
exprRTypeCheck functionList vars (Rle e1 e2)  = checkExprR functionList vars e1 e2 Rle
exprRTypeCheck functionList vars (Rge e1 e2)  = checkExprR functionList vars e1 e2 Rge

checkExprR :: [Funcao] -> [Var] -> Expr -> Expr -> (Expr -> Expr -> ExprR) -> M ExprR
checkExprR functionList vars e1 e2 operation = do
  (t1, new_e1) <- exprTypeCheck functionList vars e1
  (t2, new_e2) <- exprTypeCheck functionList vars e2
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
  
exprLTypeCheck :: [Funcao] -> [Var] -> ExprL -> M ExprL
exprLTypeCheck functionList vars (And e1 e2) = checkExprL functionList vars e1 e2 And
exprLTypeCheck functionList vars (Or e1 e2)  = checkExprL functionList vars e1 e2 Or
exprLTypeCheck functionList vars (Not e1)    = exprLTypeCheck functionList vars e1
exprLTypeCheck functionList vars (Rel e1)    = pure Rel <*> exprRTypeCheck functionList vars e1

checkExprL :: [Funcao] -> [Var] -> ExprL -> ExprL -> (ExprL -> ExprL -> b) -> M b
checkExprL functionList vars e1 e2 operation = do
  new_e1 <- exprLTypeCheck functionList vars e1
  new_e2 <- exprLTypeCheck functionList vars e2
  let new_op = operation new_e1 new_e2
  pure new_op

checkBlockVars :: [Var] -> [Var] -> M [Var]
checkBlockVars [] _ = pure []
checkBlockVars (var : xs) varList = do
  tail <- checkBlockVars xs varList
  if varFreq var varList > 1
    then do
      emitError ("Variable " ++ show var ++ " declared multiple times") varList
      pure (var : tail)
  else
    pure (var : tail)


checkBlock :: [Funcao] -> Maybe Funcao -> [Var] -> Bloco -> M Bloco
checkBlock functionList function vars (cmd : block) = do
  new_cmd <- checkCommand functionList function vars cmd
  new_block <- checkBlock functionList function vars block
  pure (new_cmd : new_block)

checkBlock _ function vars [] = pure []

checkCommand :: [Funcao] -> Maybe Funcao -> [Var] -> Comando -> M Comando

checkCommand functionList function vars (If exprL b1 b2) = do
  new_e <- exprLTypeCheck functionList vars exprL
  new_b1 <- checkBlock functionList function vars b1
  new_b2 <- checkBlock functionList function vars b2
  pure(If new_e new_b1 new_b2)

checkCommand functionList function vars (While exprL b) = do
  new_e <- exprLTypeCheck functionList vars exprL
  new_b <- checkBlock functionList function vars b
  pure(While new_e new_b)

checkCommand functionList _ vars (Imp expr) = do
  (_, new_e) <- exprTypeCheck functionList vars expr
  pure(Imp new_e)


checkCommand functionList _ vars (Atrib var expr) = do
  (eType, new_expr) <- exprTypeCheck functionList vars expr
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
      emitError ("Variable " ++ show var ++ " not declared") new_cmd
      return cmd

checkCommand _ _ vars (Leitura var) = do
  let cmd = Leitura var
  case lookupVar var vars of
    Just varType -> pure cmd
    Nothing -> do
      emitError ("Variable " ++ show var ++ " not declared") cmd
      return cmd

checkCommand functionList function vars (Ret maybeExpr) = do
  let returnType = case function of
        Just (id :->: (_, retType)) -> retType
        Nothing -> TVoid
  let cmd = Ret maybeExpr
  case maybeExpr of
    Just e -> do
      (eType, new_e) <- exprTypeCheck functionList vars e
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

checkCommand functionList function vars (Proc id exprList) =
  case lookupFunction id functionList of
    Just fun -> do
      new_exprList <- checkFunctionCall functionList vars fun fun exprList
      return (Proc id new_exprList)
    Nothing -> do
      emitError ("Function " ++ show id ++ "not declared") (Proc id exprList)
      return (Proc id exprList)

checkFunctionCall :: [Funcao] -> [Var] -> Funcao -> Funcao -> [Expr] -> M [Expr]

checkFunctionCall functionList vars originalFunction (_ :->: ([], _)) [] = pure []

checkFunctionCall functionList vars originalFunction function exprList = do
  let (funId :->: (functionVarList, returnType)) = function
  if length functionVarList < length exprList 
    then do
      emitError ("Too many arguments (" ++ show exprList ++ ") on functionCall") originalFunction
      return exprList
  else if length functionVarList > length exprList
    then do
      emitError ("Too few arguments (" ++ show exprList ++ ") on functionCall") originalFunction
      return exprList
  else do
    let (functionVar : rest) = functionVarList
    let (id :#: (varType, _mem)) = functionVar
    let (expr : exprTail) = exprList
    let nxtFunction = funId :->: (rest, returnType)
    (eType, new_expr) <- exprTypeCheck functionList vars expr
    newTail <- checkFunctionCall functionList vars originalFunction nxtFunction exprTail
    case (varType, eType) of
      (TString, TString) -> pure (new_expr : newTail)
      (TString, _) -> do
        emitError ("Cannot coerce" ++ show eType ++ "to string") originalFunction
        return (new_expr : newTail)
      (_, TString) -> do
        emitError ("Cannot coerce string to " ++ show eType) originalFunction
        return (new_expr : newTail)
      (TInt, TInt) -> pure (new_expr : newTail)
      (TDouble, TDouble) -> pure (new_expr : newTail)
      (TInt, TDouble) -> do
        emitWarning "Coercing double to int" originalFunction
        return (DoubleInt new_expr : newTail)
      (TDouble, TInt) -> do
        emitWarning "Coercing int to double" originalFunction
        return (IntDouble new_expr : newTail)
      _ -> do
        emitError "Unsuported types on functionCall" originalFunction
        return (new_expr : newTail)

functionFreq :: Funcao -> [Funcao] -> Int
functionFreq function [] = 0
functionFreq function@(id :->: (_, _)) ((id2 :->: (_, _)) : xs) 
  | id == id2 = 1 + functionFreq function xs
  | otherwise = functionFreq function xs

varFreq :: Var -> [Var] -> Int
varFreq var [] = 0
varFreq var@(id :#: (_, _)) ((id2 :#: (_, _)) : xs) 
  | id == id2 = 1 + varFreq var xs
  | otherwise = varFreq var xs

checkFunction :: [Funcao] -> Funcao -> (Id, [Var], Bloco) -> M (Funcao, (Id, [Var], Bloco))
checkFunction functionList function (id, vars, block) = do
  new_block <- checkBlock functionList (Just function) vars block
  if functionFreq function functionList > 1
    then do
      emitError ("Function " ++ show function ++ " declared multiple times") functionList
      pure (function, (id, vars, new_block))
  else 
      pure (function, (id, vars, new_block))


checkFunctionList :: [Funcao] -> [Funcao] -> [(Id, [Var], Bloco)] -> M ([Funcao], [(Id, [Var], Bloco)])
checkFunctionList _ [] [] = pure ([], [])
checkFunctionList functionList (function : functionTail) ((id, vars, block) : rest) = do
  newVars <- checkBlockVars vars vars
  (new_function, new_functionBody) <- checkFunction functionList function (id, newVars, block)
  (tail_functionList, tail_functionBodyList) <- checkFunctionList functionList functionTail rest
  pure (new_function : tail_functionList, new_functionBody : tail_functionBodyList)

checkProgram :: Programa -> M Programa
checkProgram (Prog functionList functionBodyList mainBlockVars mainBlock) = do
  newMainBlockVars <- checkBlockVars mainBlockVars mainBlockVars
  (new_functionList, new_functionBodyList) <- checkFunctionList functionList functionList functionBodyList
  new_mainBlock <- checkBlock functionList Nothing mainBlockVars mainBlock
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
