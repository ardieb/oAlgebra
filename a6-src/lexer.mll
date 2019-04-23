{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let frac = '-'? int+ '/' int+
let float = '-'? int+ '.' int+
let num = int | frac | float
let row = '[' [white num white]+ ']'
let matrix = '[' [white row white]+ ']'

rule read =
  parse
  | white {read lexbuf}
  | num { NUM ( num_of num ) }
  | matrix { MATRIX ( matrix_of matrix ) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { EQUALS }
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIV }
  | "*" { TIMES }
  | "Scale" { SCALE }
  | "Transpose { TRANSPOSE }
  | "Inverse" { INVERSE }
  | "Det" { DET }
  | "Rowspace" { ROWSPACE }
  | "Nullspace" { NULLSPACE }
  | "Colspace" { COLSPACE }
  | "Reduce" { REDUCE }
  | "Dot" { DOT }
  | eof { EOF }
