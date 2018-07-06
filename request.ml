open Lwt.Infix

type t = {
  meth: Cohttp.Code.meth;
  path: string;
  headers: Cohttp.Header.t;
  uri_parameter: (string * string option) list;
  query_parameter: (string * string list) list;
  body: string;
}

let make request body =
  let uri = Cohttp.Request.uri request in
  let meth = Cohttp.Request.meth request in
  let headers =  Cohttp.Request.headers request in
  let path = Uri.path uri in
  let uri_parameter = [] in
  let query_parameter = Uri.query uri in
  Cohttp_lwt.Body.to_string body >>= fun body ->
  Lwt.return {
    meth;
    path;
    headers;
    uri_parameter;
    query_parameter;
    body;
  }