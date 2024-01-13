%token <int> INT
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token EQ
%token PLUS
%token EOF
%left EQ
%left PLUS
%right ELSE
%type <Grammar.expr> item
%start <Grammar.expr option> prog
%%
prog: 
  | EOF { print_endline "EOF"; None }
  | i = item; EOF {Some i}
;
item:
  | num = INT 
    { Value (MyInt num) }
  | TRUE 
    { Value (MyBool true) }
  | FALSE
    { Value (MyBool false) }
  | IF; cond = item; THEN; thenExpr = item; ELSE; elseExpr = item;
    { If (cond, thenExpr, elseExpr) }
  | x = item; PLUS; y = item
    { Plus (x, y) }
  | x = item; EQ; y = item 
    { Eq (x, y) }
;