type 'a t

val empty : 'a t
val add : string -> 'a -> 'a t -> 'a t
val find_opt : string -> 'a t -> 'a option
