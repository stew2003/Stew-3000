open Printf

(* [effect] produces a string which has the effect specified
    by the given ANSI color code applied to it *)
let effect (code : string) (s : string) = sprintf "\027[%sm%s\027[0m" code s

let bold s = effect "1" s

let red s = effect "31" s

let br_red s = effect "31;1" s

let br_cyan s = effect "36;1" s

let br_green s = effect "32;1" s

let br_blue s = effect "94" s

let br_black s = effect "90" s

let br_yellow s = effect "33;1" s

let white s = effect "37" s

let magenta s = effect "35" s

let error s = bold (br_red s)

let success s = bold (br_green s)

let log s = bold (br_blue s)

let warn s = bold (br_yellow s)
