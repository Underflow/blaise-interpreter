%token <string> VAR
%token <int> INT
%token ADD MUL DIV MIN LT GT LEQ GEQ CMPEQ NOT AND OR
%token LPAREN RPAREN RB LB EQ SC COM
%token MAIN IF WHILE ELSE RETURN VARS
%token EOF
%left LT GT LEQ GEQ CMPEQ
%left ADD MIN AND OR
%left MUL DIV
%nonassoc UMIN NOT
%start prg
%type <Ast.prg> prg
%%
prg:
fdecls main EOF                { {Ast.func = $1; Ast.main = $2} };

expr:
  VAR                   { Ast.Var $1 }
  | INT                 { Ast.Int $1 }
  | expr ADD expr       { Ast.BinOp($1,"+",$3) }
  | expr MUL expr       { Ast.BinOp($1,"*",$3) }
  | expr MIN expr       { Ast.BinOp($1,"-",$3) }
  | expr DIV expr       { Ast.BinOp($1,"/",$3) }
  | expr LT expr        { Ast.BinOp($1,"<",$3) }
  | expr GT expr        { Ast.BinOp($1,">",$3) }
  | expr LEQ expr       { Ast.BinOp($1,"<=",$3) }
  | expr GEQ expr       { Ast.BinOp($1,">=",$3) }
  | expr CMPEQ expr     { Ast.BinOp($1,"==",$3) }
  | expr AND expr       { Ast.BinOp($1,"&&",$3) }
  | expr OR expr        { Ast.BinOp($1,"||",$3) }
  | MIN expr %prec UMIN { Ast.UniOp("-",$2) }
  | NOT expr            { Ast.UniOp("!",$2) }
  | VAR params
      { Ast.Call ($1,$2) }
  | LPAREN expr RPAREN  { $2 }
;

params:
  | LPAREN RPAREN       { [] }
  | LPAREN elist RPAREN { $2 }
;

elist:
  | expr                { $1::[] }
  | expr COM elist      { $1::$3 }
;

stm:
  | VAR EQ expr SC      { Ast.Assign ($1,$3) }
  | expr SC             { Ast.Expr $1 }
  | IF LPAREN expr RPAREN stmblock
      { Ast.If ($3, $5, []) }
  | IF LPAREN expr RPAREN stmblock ELSE stmblock
      { Ast.If ($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmblock
      { Ast.While ($3, $5) }
  | RETURN expr SC      { Ast.Return $2 }

stmblock:
  | LB RB               { [] }
  | LB stmlist RB       { $2 }
;

stmlist:
  | stm                 { $1::[] }
  | stm stmlist         { $1::$2 }
;

vars:
  | VARS vlist SC       { $2 }
;

vlist:
  | VAR                 { $1::[] }
  | VAR COM vlist       { $1::$3 }
;

fparam:
  | LPAREN RPAREN       { [] }
  | LPAREN vlist RPAREN { $2 }
;

body:
  | LB RB               { ([],[]) }
  | LB stmlist RB       { ([],$2) }
  | LB vars stmlist RB  { ($2,$3) }

fdecl:
  | VAR fparam body
      {
        let (v,b) = $3 in
        {
          Ast.fname   = $1;
          Ast.fparams = $2;
          Ast.fvars   = v;
          Ast.fbody   = b;
        }
      }
;

fdecls:
  |                     { [] }
  | fdecl fdecls        { $1::$2 }
;

main:
  | MAIN LPAREN RPAREN body
      {
        let (v,b) = $4 in
        {
          Ast.mainvars = v;
          Ast.mainbody = b;
        }
      }
;
