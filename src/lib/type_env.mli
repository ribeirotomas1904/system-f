type 'a t

val empty : 'a t
val find_variable_opt : string -> 'a t -> 'a option
val is_type_variable_bounded : string -> 'a t -> bool
val add_variable : string -> 'a -> 'a t -> 'a t
val add_type_variable : string -> 'a t -> 'a t
