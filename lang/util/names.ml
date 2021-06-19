(* [gensym] produces a globally unique name, given a base.
    credit to cs126 for this implementation. *)
let gensym : string -> string =
  let counter = ref 0 in
  fun (base : string) ->
    let number = !counter in
    counter := !counter + 1;
    Printf.sprintf "%s_%d" base number

(* [function_label] produces a unique label for a given
    function, given its name. *)
let function_label (base : string) : string = Printf.sprintf "function_%s" base
