open Ast

type value =
  | V_int of int
  | V_closure of { parameter : string; body : expression; env : value Env.t }

let string_of_value value =
  match value with V_int i -> string_of_int i | V_closure _ -> "<fun>"

let rec eval env expression =
  match expression with
  | E_int i -> V_int i
  | E_variable ident -> (
      let value_opt = Env.find_opt ident env in
      match value_opt with Some v -> v | None -> failwith "TODO")
  | E_abstraction { parameter; body; _ } -> V_closure { parameter; body; env }
  | E_application { function_; argument } -> (
      let argument_value = eval env argument in
      let closure = eval env function_ in
      match closure with
      | V_closure { parameter; body; env = closure_env } ->
          let closure_env' = Env.add parameter argument_value closure_env in
          eval closure_env' body
      | _ -> failwith "TODO")
  | E_type_abstraction { body; _ } -> eval env body
  | E_type_application { function_; _ } -> eval env function_
  | E_let_in { parameter; argument; body } ->
      let argument_value = eval env argument in
      let env' = Env.add parameter argument_value env in
      eval env' body
