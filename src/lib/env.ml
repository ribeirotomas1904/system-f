module StringMap = Map.Make (String)

type 'a t = 'a StringMap.t

let empty = StringMap.empty
let add = StringMap.add
let find_opt = StringMap.find_opt
