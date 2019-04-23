%{
  open Ast
%}

%token <Ast.num> NUM
%token <Ast.matrix> MATRIX
%token EQUALS
%token TIMES
%token SCALE
%token PLUS
%token MINUS
%token DIV
%token TRANSPOSE
%token INVERSE
%token DET
%token ROWSPACE
%token COLSPACE 
%token NULLSPACE
%token REDUCE
%token DOT
%token LPAREN
%token RPAREN
%token EOF

%nonassoc TRANSPOSE
%nonassoc INVERSE
%nonassoc DET
%nonassoc ROWSPACE
%nonassoc COLSPACE
%nonassoc NULLSPACE
%nonassoc REDUCE
%left PLUS
%left TIMES
%left DOT
%left DIV

%start <Ast.expr> prog
%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | n = NUM  { Num n }
  | m = MATRIX { Matrix m }
  | TRANSPOSE; e = expr { Unary (Transpose, e) }
  | INVERSE; e = expr { Unary (Inverse, e) }
  | DET; e = expr { Unary (Det, e) }
  | ROWSPACE; e = expr { Unary (Rowspace, e) }
  | NULLSPACE; e = expr { Unary (Nullspace, e) }
  | COLSPACE; e = expr { Unary (Colspace, e) }
  | REDUCE; e = expr { Unary (Reduce, e) }
  | e1 = expr; TIMES; e2 = expr { Binary (e1, Mul, e2) }
  | e1 = expr; DIV; e2 = expr { Binary (e1, Div, e2) }
  | e1 = expr; MINUS; e2 = expr { Binary (e1, Sub, e2) }
  | e1 = expr; PLUS; e2 = expr { Binary (e1, Add, e2) }
  | e1 = expr; DOT; e2 = expr { Binary (e1, Dot, e2) }
  | e1 = expr; EQUALS; e2 = expr { Binary (e1, Solve, e2) }
  | e1 = expr; SCALE; e2 = expr { Binary (e1, Scale, e2) }
  | LPAREN; e = expr; RPAREN { e }
  ;