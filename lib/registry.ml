open Core

module type BUILTIN = sig
  val name : string
  val arity : int option
  val doc : string
  val apply : Value.value list -> Value.value
end

let registered : (string, (module BUILTIN)) Hashtbl.t =
  Hashtbl.create (module String)

let register (module B : BUILTIN) : unit =
  Hashtbl.set registered ~key:B.name ~data:(module B : BUILTIN)

let mem (name : string) : bool = Hashtbl.mem registered name

let find (name : string) : (module BUILTIN) option =
  Hashtbl.find registered name

let find_exn (name : string) : (module BUILTIN) =
  Hashtbl.find_exn registered name

let all () : (module BUILTIN) list = Hashtbl.data registered

let to_prim (name : string) : Value.value =
  let (module B : BUILTIN) = find_exn name in
  Val_prim B.apply

let names () : string list =
  Hashtbl.keys registered |> List.sort ~compare:String.compare
