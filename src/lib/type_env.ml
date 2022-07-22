module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type 'a t = { variables : 'a StringMap.t; type_variables : StringSet.t }

let empty = { variables = StringMap.empty; type_variables = StringSet.empty }
let find_variable_opt key { variables; _ } = StringMap.find_opt key variables

let is_type_variable_bounded element { type_variables; _ } =
  StringSet.mem element type_variables

let add_variable key type_ ({ variables; _ } as env) =
  { env with variables = StringMap.add key type_ variables }

let add_type_variable element ({ type_variables; _ } as env) =
  { env with type_variables = StringSet.add element type_variables }
