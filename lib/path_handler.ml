type meth = Cohttp.Code.meth

type request_handler = Request.t -> Response.t

type t = {
  meth: meth;
  path_mapping: Path_mapping.t;
  request_handler: request_handler;
}

let create meth path_mapping request_handler = {
    meth;
    path_mapping;
    request_handler;
}

let handle_path_handler path_handler request =
  let path_mapping = path_handler.path_mapping in
  let request = Request.add_path_parameters request path_mapping in
  path_handler.request_handler request

let apply_path_handler path_handler request default_request_handler =
  match path_handler with
  | Some path_handler -> handle_path_handler path_handler request
  | None -> default_request_handler request
