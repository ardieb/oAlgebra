{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let frac = '-'? int+ '/' int+
let float = '-'? int+ '.' int+
let number = int | frac | float
let row = '[' (' '? number ' '?)+ ']'
let mat = '[' (' '? row ';'? ' '?)+ ']'
let variable = ['A'-'Z']

rule read =
  parse
  | white {read lexbuf}
  | number { NUM ( Ast.num_of (Lexing.lexeme lexbuf) ) }
  | mat { MATRIX ( Ast.matrix_of (Lexing.lexeme lexbuf) ) }
  | variable { VAR }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { EQUALS }
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIV }
  | "*" { TIMES }
  | "scale" { SCALE }
  | "transpose" { TRANSPOSE }
  | "inverse" { INVERSE }
  | "det" { DET }
  | "qr factor" { QRFACTOR }
  | "rowspace" { ROWSPACE }
  | "nullspace" { NULLSPACE }
  | "colspace" { COLSPACE }
  | "reduce" { REDUCE }
  | "dot" { DOT }
  | "change basis to" { CHANGEBASIS }
  | "projected onto" { ORTHPROJECT }
  | "decomposed on" { DECOMP }
  | "distance to" { DISTTOBASIS }
  | eof { EOF }
