type 'a t

(* GETTERS *)
val empty : 'a t

(* FUNCTIONS *)
val search : 'a -> 'a t -> bool
val insert : 'a -> 'a t -> 'a t
val t_min : 'a t -> 'a option
val t_max : 'a t -> 'a option
val delete : 'a -> 'a t -> 'a t
