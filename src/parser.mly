%{
  open Ast
%}

%token <Ast.value> NUM
%token <Ast.matrix> MATRIX
%token VAR
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
%token CHANGEBASIS
%token ORTHPROJECT
%token DISTTOBASIS
%token QRFACTOR
%token DECOMP
%token LUDECOMP
%token REDUCE
%token DOT
%token LPAREN
%token RPAREN
%token EOF

%nonassoc DECOMP
%nonassoc LUDECOMP
%nonassoc QRFACTOR


%left TRANSPOSE
%left INVERSE
%left DET
%left ROWSPACE
%left COLSPACE
%left NULLSPACE
%left REDUCE
%left CHANGEBASIS
%left ORTHPROJECT
%left DISTTOBASIS
%left PLUS
%left MINUS
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
  | QRFACTOR; e = expr { Unary (QRFactor, e) }
  | LUDECOMP; e = expr { Unary (LuDecomp, e) }
  | e1 = expr; TIMES; e2 = expr { Binary (e1, Mul, e2) }
  | e1 = expr; DIV; e2 = expr { Binary (e1, Div, e2) }
  | e1 = expr; MINUS; e2 = expr { Binary (e1, Sub, e2) }
  | e1 = expr; PLUS; e2 = expr { Binary (e1, Add, e2) }
  | e1 = expr; DOT; e2 = expr { Binary (e1, Dot, e2) }
  | e1 = expr; TIMES; VAR; EQUALS; e2 = expr { Binary (e1, Solve, e2) }
  | e1 = expr; SCALE; e2 = expr { Binary (e1, Scale, e2) }
  | e1 = expr; CHANGEBASIS; e2 = expr { Binary (e1, ChangeBasis, e2) }
  | e1 = expr; ORTHPROJECT; e2 = expr { Binary (e1, OrthProject, e2) }
  | e1 = expr; DISTTOBASIS; e2 = expr { Binary (e1, DistToBasis, e2) }
  | e1 = expr; DECOMP; e2 = expr { Binary (e1, Decomp, e2) }
  | LPAREN; e = expr; RPAREN { e }
  ;