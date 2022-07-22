%{
  open Ast
%}

%token <int> INT
%token <string> IDENT
%token <string> UPPER_IDENT

%token T_INT

%token EQUALS
%token LET
%token IN
%token TYPE
%token FUN
%token COLON
%token ARROW
%token ASTERISK
%token LEFT_PAREN
%token RIGHT_PAREN
%token DOT
%token FORALL
%token EOF

%start <Ast.expression option> program
%%

program:
  | EOF
    { None }
  | e = expression; EOF
    { Some e }
  ;

expression:
  | LET; ident = IDENT; EQUALS; abstraction = abstraction; IN; expression = expression
    {
      E_let_in { parameter = ident; argument = abstraction; body = expression }
    }
  | TYPE; ident = UPPER_IDENT; EQUALS; type_ = type_; IN; expression = expression
    {
      E_type_application {
        function_ =
          E_type_abstraction {
            parameter = ident;
            body = expression;
          };
        argument = type_;
      }
    }
  | abstraction = abstraction { abstraction }
  ;

abstraction:
  | FUN; LEFT_PAREN; ident = IDENT; COLON; type_ = type_; RIGHT_PAREN; ARROW; expression = abstraction
    {
      E_abstraction {
        parameter = ident;
        parameter_type = type_;
        body = expression;
      }
    }
  | FUN; LEFT_PAREN; ident = UPPER_IDENT; COLON; ASTERISK; RIGHT_PAREN; ARROW; expression = abstraction
    {
      E_type_abstraction {
        parameter = ident;
        body = expression;
      }
    }
  | application = application { application }
  ;

application:
  | function_ = application; argument = atom
    {
      E_application {
        function_;
        argument;
      }
    }
  | function_ = application; argument = type_
    {
      E_type_application {
        function_;
        argument;
      }
    }
  | atom = atom { atom }
  ;

atom:
  | i = INT
    { E_int i }
  | ident = IDENT
    { E_variable ident }
  | LEFT_PAREN; expression = expression; RIGHT_PAREN
    { expression }
  ;

type_:
  | parameter_type = type_atom; ARROW; return_type = type_
    {
      T_arrow { parameter_type; return_type }
    }
  | FORALL; ident = UPPER_IDENT; DOT; type_ = type_
    {
      T_forall { parameter = ident; type_ }
    }

  | type_atom = type_atom { type_atom }
  ;

type_atom:
  | ident = UPPER_IDENT
    { T_variable ident }
  | T_INT
    { T_int }
  | LEFT_PAREN; type_ = type_; RIGHT_PAREN
    { type_ }
  ;

// expression =
//   | "let" IDENT "=" abstraction "in" expression
//   | "type" UPPER_IDENT "=" type "in" expression
//   | abstraction

// abstraction =
//   | "fun" "(" IDENT ":" type ")" "->" abstraction
//   | "fun" "(" UPPER_IDENT ":" "*" ")" "->" abstraction
//   | application

// application =
//   | application atom
//   | application type
//   | atom

// atom =
//   | INT
//   | IDENT
//   | "(" expression ")"



// type =
//   | type_atom -> type
//   | "forall" UPPER_IDENT "." type
//   | type_atom

// type_atom =
//   | UPPER_IDENT
//   | "(" type ")"
