type t = {
  name: string;
  regex: Re.t
}

let create_part regex = {
  name = "";
  regex;
}

let create_var name regex = {
  name;
  regex;
}

let part path_part = create_part (Re.str path_part)

let anytihng = Re.rep1 (Re.compl [Re.char '/'])

let var name = create_var name anytihng

let separator = create_part (Re.char '/')

let split_into_path_parts path =
  let path_separator = Re.compile separator.regex in
  Re.split path_separator path