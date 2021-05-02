open Printf

(* [effect] produces a string which has the effect specified
    by the given ANSI color code applied to it *)
let effect (code : string) (s : string) : string =
  sprintf "\027[%sm%s\027[0m" code s

let bold (s : string) : string = effect "1" s

let red (s : string) : string = effect "31" s

let br_red (s : string) : string = effect "31;1" s

let br_cyan (s : string) : string = effect "36;1" s

let br_green (s : string) : string = effect "32;1" s

let error (s : string) : string = bold (br_red s)

let success (s : string) : string = bold (br_green s)
