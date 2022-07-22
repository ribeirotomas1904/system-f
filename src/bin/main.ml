open Lambda_core

let parse input =
  let lexbuf = Lexing.from_string input in
  Parser.program Lexer.read lexbuf

let input =
  {|

  let x = 42 in

  let id =
    fun (B: *) ->
      fun (x: B) ->
        x
  in

  let apply_id =
    fun (id: forall A. A -> A) ->
      fun (B: *) ->
        fun (x: B) ->
          id B x
  in

  apply_id id Int x

  |}

let ast = parse input |> Option.get

let () =
  ast
  |> Type_checker.check Type_env.empty
  |> Ast.string_of_type
  |> Printf.printf "TYPE = %s\n"

let () =
  ast |> Interpreter.eval Env.empty |> Interpreter.string_of_value
  |> Printf.printf "VALUE = %s\n"
