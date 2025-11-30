(** Module signature that all builtins must implement *)
module type BUILTIN = sig
  val name : string
  val arity : int option
  val doc : string
  val apply : Value.value list -> Value.value
end

val register : (module BUILTIN) -> unit
val mem : string -> bool
val find : string -> (module BUILTIN) option
val find_exn : string -> (module BUILTIN)
val all : unit -> (module BUILTIN) list
val to_prim : string -> Value.value
val names : unit -> string list
