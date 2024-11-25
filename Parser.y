{

module Parser where
import Token
import qualified AST as AST
import qualified Lex as L

}


%name eval
%tokentype { Token }
%error { parseError }
%token
  TInt {TInt}
  TDouble {TDouble}
  TString {TString}
  TVoid {TVoid}
  Add {Add}
  Sub {Sub}
  Mul {Mul}
  Div {Div}
  LPAR {LPAR}
  RPAR {RPAR}
  LBRACE {LBRACE}
  RBRACE {RBRACE}
  Comma {Comma}
  EndCommand {EndCommand}
  And {And}
  Or {Or}
  Gt {Gt}
  Ge {Ge}
  Le {Le}
  Lt {Lt}
  Not {Not}
  Eq {Eq}
  Diff {Diff}
  Assign {Assign}
  Id {Id $$}
  If {If}
  Else {Else}
  While {While}
  Read {Read}
  Print {Print}
  Return {Return}
  CInt {CInt $$}
  CDouble {CDouble $$}
  CString {CString $$}


%%

Programa : FunctionList MainBlock               {let (decls, cmds) = $2 in
                                                let (headers, funs) = $1 in
                                                AST.Prog headers funs decls cmds}
         | MainBlock                            {let (decls, cmds) = $1 in
                                                AST.Prog [] [] decls cmds}

FunctionList : Function FunctionList            {AST.concatFunction $1 $2}
             | Function                         {AST.concatFunction $1 ([], [])}

MainBlock: LBRACE DeclarationList CommandList RBRACE {($2, $3)}
         | LBRACE CommandList RBRACE                 {([], $2)}
         | LBRACE DeclarationList RBRACE             {($2, [])}

Function : Tipo Id LPAR ParameterList RPAR MainBlock {
                                                      let (decls, cmds) = $6 in
                                                      AST.createFunction $1 $2 $4 decls cmds}
         | Tipo Id LPAR RPAR MainBlock               {
                                                      let (decls, cmds) = $5 in
                                                      AST.createFunction $1 $2 [] decls cmds}

Tipo : TInt                                     {AST.TInt}
     | TDouble                                  {AST.TDouble}
     | TString                                  {AST.TString}
     | TVoid                                    {AST.TVoid}

ParameterList: Parameter Comma ParameterList     {$1 : $3}
             | Parameter                         {[$1]}

Parameter: Tipo Id                               {$2 AST.:#: ($1, 0)}

DeclarationList: Declaration DeclarationList       {$1 ++ $2}
               | Declaration                       {$1}

Declaration: Tipo IdList EndCommand               {map (\id -> id AST.:#: ($1, 0)) $2 }

IdList: Id Comma IdList                           {$1 : $3}
      | Id                                        {[$1]}

Block: LBRACE CommandList RBRACE                     {$2}

CommandList: Command CommandList                     {$1 : $2}
           | Command                                 {[$1]}

Command: IfCommand                                             {$1} 
       | WhileCommand                                          {$1}
       | AssignCommand EndCommand                              {$1}
       | PrintCommand EndCommand                               {$1}
       | ReadCommand  EndCommand                               {$1}
       | FunctionCallCommand EndCommand                        {$1}
       | ReturnCommand EndCommand                              {$1}

IfCommand: If LPAR BooleanExpr RPAR Block                      {AST.If $3 $5 []}
         | If LPAR BooleanExpr RPAR Block Else Block           {AST.If $3 $5 $7}

WhileCommand: While BooleanExpr Block                {AST.While $2 $3}

AssignCommand: Id Assign Expr              {AST.Atrib $1 $3}

PrintCommand: Print LPAR Expr RPAR         {AST.Imp $3} 

ReadCommand: Read LPAR Id RPAR                       {AST.Leitura $3}

FunctionCallCommand: Id LPAR FunctionCallParameterList RPAR {AST.Proc $1 $3}
                   | Id LPAR RPAR                           {AST.Proc $1 []}


FunctionCallParameterList: Expr Comma FunctionCallParameterList           {$1 : $3}
                         | CString Comma FunctionCallParameterList        {(AST.Lit $1) : $3}
                         | Expr                                           {[$1]}

ReturnCommand: Return Expr                     {AST.Ret (Just $2)}
             | Return                          {AST.Ret Nothing}

BooleanExpr : BooleanExpr And BooleanTerm      {AST.And $1 $3}
            | BooleanExpr Or BooleanTerm       {AST.Or $1 $3}
            | Not BooleanTerm                  {AST.Not $2}
            | BooleanTerm                      {$1}
            
BooleanTerm : Expr Gt Expr                     {AST.Rel (AST.Rgt $1 $3)}
            | Expr Le Expr                     {AST.Rel (AST.Rle $1 $3)}
            | Expr Ge Expr                     {AST.Rel (AST.Rge $1 $3)}
            | Expr Lt Expr                     {AST.Rel (AST.Rlt $1 $3)}
            | Expr Diff Expr                   {AST.Rel (AST.Rdif $1 $3)} 
            | Expr Eq Expr                     {AST.Rel (AST.Req $1 $3)} 
            | LPAR BooleanExpr RPAR            {$2}

Expr : Expr Add Term                           {AST.Add $1 $3}
      | Expr Sub Term                          {AST.Sub $1 $3}
      | Sub Expr                               {AST.Neg $2}
      | Term                                   {$1}

Term  : Term  Mul Factor                                           {AST.Mul $1 $3}
      | Term Div Factor                                            {AST.Div $1 $3}
      | Factor                                                     {$1}

Factor : CDouble                                                   {AST.Const (AST.CDouble $1)}
       | CInt                                                      {AST.Const (AST.CInt $1)}
       | Id                                                        {AST.IdVar $1}
       | LPAR Expr RPAR                                            {$2}
       | Id LPAR FunctionCallParameterList RPAR                    {AST.Chamada $1 $3}
       | Id LPAR RPAR                                              {AST.Chamada $1 []}
       | CString                                                   {AST.Lit $1}

{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

makeAST code = eval (L.alexScanTokens code)

main :: IO ()
main = do
    putStrLn "Digite o caminho do arquivo com o código:"
    path <- getLine
    code <- readFile path
    writeFile "AST.txt" (show $ makeAST code)
}
