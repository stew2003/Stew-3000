module Env = Map.Make (struct
  type t = string

  let compare = compare
end)

type 'a env = 'a Env.t

include Env

let of_list l = l |> List.to_seq |> of_seq
