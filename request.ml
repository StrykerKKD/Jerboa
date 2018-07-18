open Lwt.Infix

type t = {
  meth: Cohttp.Code.meth;
  path: string;
  headers: Cohttp.Header.t;
  path_parameter: (string * string option) list;
  query_parameter: (string * string list) list;
  body: string;
}

let make request body =
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

let check_zip accum _ = accum

let find_path_parameter zipped =
  Base.List.fold zipped ~init:[] ~f:check_zip

let update_path_parameter request path_parameter =
  {request with path_parameter = path_parameter;}

let add_path_parameter request path_handler =
  let path_parts = Path.get_path_parts request.path in
  let zipped = Base.List.zip path_handler path_parts in
  let path_parameter_option = Base.Option.map zipped ~f:find_path_parameter in
  let path_parameter = Base.Option.value path_parameter_option ~default:[] in
  {request with path_parameter = path_parameter;}