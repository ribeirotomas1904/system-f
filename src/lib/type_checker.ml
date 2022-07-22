open Ast

let rec find_unbounded_type_variable_opt env type_ =
  match type_ with
  | T_int -> None
  | T_arrow { parameter_type; return_type } -> (
      let unbounded_type_variable_opt =
        find_unbounded_type_variable_opt env parameter_type
      in
      match unbounded_type_variable_opt with
      | Some ident -> Some ident
      | None -> find_unbounded_type_variable_opt env return_type)
  | T_variable ident when Type_env.is_type_variable_bounded ident env -> None
  | T_variable ident -> Some ident
  | T_forall { parameter; type_ } ->
      let env' = Type_env.add_type_variable parameter env in
      find_unbounded_type_variable_opt env' type_

let rec type_substitution type_ ~from ~to_ =
  match type_ with
  | T_int -> T_int
  | T_arrow { parameter_type; return_type } ->
      T_arrow
        {
          parameter_type = type_substitution parameter_type ~from ~to_;
          return_type = type_substitution return_type ~from ~to_;
        }
  | T_variable ident when ident = from -> to_
  | T_variable ident -> T_variable ident
  | T_forall { parameter; type_ } when parameter = from ->
      T_forall { parameter; type_ }
  | T_forall { parameter; type_ } ->
      T_forall { parameter; type_ = type_substitution type_ ~from ~to_ }

(* TODO: refactor using type substitution *)
let are_types_equals a b =
  let rec aux alpha a b =
    match (a, b) with
    | T_int, T_int -> true
    | ( T_arrow
          { parameter_type = parameter_type_a; return_type = return_type_a },
        T_arrow
          { parameter_type = parameter_type_b; return_type = return_type_b } )
      ->
        aux alpha parameter_type_a parameter_type_b
        && aux alpha return_type_a return_type_b
    | T_variable ident_a, T_variable ident_b -> (
        match List.assoc_opt ident_a alpha with
        | Some x -> x = ident_b
        | None -> (
            let alpha' = List.map (fun (a, b) -> (b, a)) alpha in
            match List.assoc_opt ident_b alpha' with
            | Some _ -> false
            | None -> ident_a = ident_b))
    | ( T_forall { parameter = parameter_a; type_ = type_a },
        T_forall { parameter = parameter_b; type_ = type_b } ) ->
        let alpha' =
          if parameter_a = parameter_b then alpha
          else (parameter_a, parameter_b) :: alpha
        in

        aux alpha' type_a type_b
    | _ -> false
  in
  aux [] a b

let rec check env expression =
  match expression with
  | E_int _ -> T_int
  | E_variable ident -> (
      Type_env.find_variable_opt ident env |> function
      | None -> failwith (Printf.sprintf "ERROR: unbounded variable %s" ident)
      | Some t -> t)
  | E_abstraction { parameter; parameter_type; body } ->
      (* (let unbounded_type_variable_opt =
           find_unbounded_type_variable_opt env parameter_type
         in
         match unbounded_type_variable_opt with
         | None -> *)
      let env' = Type_env.add_variable parameter parameter_type env in
      let body_type = check env' body in
      T_arrow { parameter_type; return_type = body_type }
      (* | Some ident ->
          failwith (Printf.sprintf "ERROR: unbounded type variable %s" ident) ) *)
  | E_application { function_; argument } -> (
      let function_type = check env function_ in
      let argument_type = check env argument in
      match function_type with
      | T_arrow { parameter_type; return_type } ->
          if are_types_equals parameter_type argument_type then return_type
          else
            failwith
              (Printf.sprintf "ERROR: expected type %s but instead got %s"
                 (string_of_type parameter_type)
                 (string_of_type argument_type))
      | T_int | T_variable _ | T_forall _ ->
          failwith
            "ERROR: you can only apply functions (T_arrow) on expressions")
  | E_type_abstraction { parameter; body } ->
      let env' = Type_env.add_type_variable parameter env in
      let body_type = check env' body in
      T_forall { parameter; type_ = body_type }
  | E_type_application { function_; argument } -> (
      let function_type = check env function_ in
      match function_type with
      | T_forall { parameter; type_ } ->
          type_substitution type_ ~from:parameter ~to_:argument
      | T_int | T_arrow _ | T_variable _ ->
          failwith "ERROR: you can only apply functions (T_forall) on types")
  | E_let_in { parameter; argument; body } ->
      let argument_type = check env argument in
      let env' = Type_env.add_variable parameter argument_type env in
      check env' body
