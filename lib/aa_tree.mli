type 'a t

(* GETTERS *)
val empty : 'a t

(* FUNCTIONS *)
val search : 'a -> 'a t -> bool
val insert : 'a -> 'a t -> 'a t
val delete : 'a -> 'a t -> 'a t
val successor : 'a -> 'a t -> 'a option
val predecessor : 'a -> 'a t -> 'a option
val min : 'a t -> 'a option
val max : 'a t -> 'a option
