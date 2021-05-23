module NumEnv = Map.Make (struct
  type t = int

  let compare = compare
end)

type 'a num_env = 'a NumEnv.t

include NumEnv

let of_list l = l |> List.to_seq |> of_seq
