{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let frac = '-'? int+ '/' int+
let float = '-'? int+ '.' int+
let number = int | frac | float
let row = '[' (white number white)+ ']'
let mat = '[' (white row white)+ ']'

rule read =
  parse
  | white {read lexbuf}
  | number { NUM ( Ast.num_of (Lexing.lexeme lexbuf) ) }
  | mat { MATRIX ( Ast.matrix_of (Lexing.lexeme lexbuf) ) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { EQUALS }
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIV }
  | "*" { TIMES }
  | "Scale" { SCALE }
  | "Transpose" { TRANSPOSE }
  | "Inverse" { INVERSE }
  | "Det" { DET }
  | "Rowspace" { ROWSPACE }
  | "Nullspace" { NULLSPACE }
  | "Colspace" { COLSPACE }
  | "Reduce" { REDUCE }
  | "Dot" { DOT }
  | eof { EOF }
