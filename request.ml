type t = {
  meth: Cohttp.Code.meth;
  path: string;
  headers: Cohttp.Header.t;
  path_parameter: (string * string) list;
  query_parameter: (string * string list) list;
  body: string;
}

let create request body =
  let open Lwt.Infix in
  let uri = Cohttp.Request.uri request in
  let meth = Cohttp.Request.meth request in
  let headers =  Cohttp.Request.headers request in
  let path = Uri.path uri in
  let path_parameter = [] in
  let query_parameter = Uri.query uri in
  Cohttp_lwt.Body.to_string body >>= fun body ->
  Lwt.return {
    meth;
    path;
    headers;
    path_parameter;
    query_parameter;
    body;
  }

let find_path_parameter accumulator path_and_path_part =
  let open Path in
  let path, path_part = path_and_path_part in
  if not (Base.String.is_empty path.name) then
    let path_regex = Re.compile path.regex in
    let match_group = Re.exec path_regex path_part in
    let first_match = Re.Group.get match_group 0 in
    Base.List.cons (path.name, first_match) accumulator
  else
   accumulator

let find_path_parameters path_and_path_part =
  Base.List.fold path_and_path_part ~init:[] ~f:find_path_parameter

let add_path_parameters request path_handler =
  let path_parts = Path.split_into_path_parts request.path in
  let path_handler_and_parts = Base.List.zip path_handler path_parts in
  let path_parameter_option = Base.Option.map path_handler_and_parts ~f:find_path_parameters in
  let path_parameter = Base.Option.value path_parameter_option ~default:[] in
  {request with path_parameter = path_parameter;}