type t = {
  name: string;
  regex: Re.t
}

let make ?(name = "") regex = {
  name;
  regex;
}

let str ?(name = "") part = make ~name (Re.str part)

let anytihng = Re.rep1 (Re.compl [Re.char '/'])

let any ?(name = "") () = make ~name anytihng

let sep = make (Re.char '/')

let get_path_parts path =
  let path_separator = Re.compile sep.regex in
  Re.split path_separator  path