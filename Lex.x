{
module Lex where
import Token
}

%wrapper "basic"

$digit = [0-9]          -- digits
$alpha = [a-zA-Z]
@double = $digit+(\.$digit+)
@int = $digit+
@string = \"[^\n\"]*\"
@id = ($alpha | \_)($alpha | $digit | \_)*


tokens :-

<0> $white+ ;
<0> @double {\s -> CDouble (read s)}
<0> @int {\s -> CInt (read s)}
<0> @string {\s -> CString (init (tail s))}
<0> "int" {\s -> TInt}
<0> "double" {\s -> TDouble}
<0> "string" {\s -> TString}
<0> "void" {\s -> TVoid}
<0> "+" {\s -> Add}  
<0> "-" {\s -> Sub}  
<0> "*" {\s -> Mul}  
<0> "/" {\s -> Div}  
<0> "(" {\s -> LPAR}  
<0> ")" {\s -> RPAR}  
<0> "{" {\s -> LBRACE}  
<0> "}" {\s -> RBRACE}  
<0> "," {\s -> Comma}
<0> ";" {\s -> EndCommand}
<0> "&&" {\s -> And}
<0> "||" {\s -> Or}
<0> ">" {\s -> Gt}
<0> ">=" {\s -> Ge}
<0> "<" {\s -> Le}
<0> "<=" {\s -> Lt}
<0> "!" {\s -> Not}
<0> "==" {\s -> Eq}
<0> "!=" {\s -> Diff}
<0> "=" {\s -> Assign}
<0> "if" {\s -> If}
<0> "else" {\s -> Else}
<0> "while" {\s -> While}
<0> "read" {\s -> Read}
<0> "print" {\s -> Print}
<0> "return" {\s -> Return}
<0> @id {\s -> Id s}
<0> . { \s -> error ("lexical error: " ++ show s) }


{
main :: IO ()
main = do
    putStrLn "Digite o caminho do arquivo com o código:"
    path <- getLine
    code <- readFile path
    print (alexScanTokens code)
}
