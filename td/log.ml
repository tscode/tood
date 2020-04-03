
let info_str str = "< " ^ str ^ " >"
let warn_str str = "warning: " ^ str ^ "."
let err_str  str = "error: " ^ str ^ "."

let info str = info_str str |> print_endline
let warn str = warn_str str |> print_endline
let err str  = err_str str  |> prerr_endline

