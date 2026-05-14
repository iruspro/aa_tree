type 'a t = Nil | Cons of { left : 'a t; right : 'a t; key : 'a; level : int }

(* GETTERS *)
val empty : 'a t

(* FUNCTIONS *)
val search : 'a -> 'a t -> bool
val insert : 'a -> 'a t -> 'a t
val t_min : 'a t -> 'a option
val t_max : 'a t -> 'a option
val delete : 'a -> 'a t -> 'a t

(* PRINT *)
val to_string : ('a -> string) -> 'a t -> string
val print : ('a -> string) -> 'a t -> unit
