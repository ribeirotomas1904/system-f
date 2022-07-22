type type_ =
  | T_int
  | T_arrow of { parameter_type : type_; return_type : type_ }
  | T_variable of string
  | T_forall of { parameter : string; type_ : type_ }

type expression =
  | E_int of int
  | E_variable of string
  | E_abstraction of {
      parameter : string;
      parameter_type : type_;
      body : expression;
    }
  | E_application of { function_ : expression; argument : expression }
  | E_type_abstraction of { parameter : string; body : expression }
  | E_type_application of { function_ : expression; argument : type_ }
  | E_let_in of { parameter : string; argument : expression; body : expression }

let rec string_of_type t =
  match t with
  | T_int -> "Int"
  | T_arrow { parameter_type; return_type } ->
      let parameter_type_str =
        match parameter_type with
        | T_arrow _ | T_forall _ ->
            Printf.sprintf "(%s)" (string_of_type parameter_type)
        | T_int | T_variable _ -> string_of_type parameter_type
      in
      (* TODO: should i show a "forall" on the right between parens? *)
      Printf.sprintf "%s -> %s" parameter_type_str (string_of_type return_type)
  | T_variable ident -> ident
  | T_forall { parameter; type_ } ->
      Printf.sprintf "forall %s. %s" parameter (string_of_type type_)
