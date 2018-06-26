open Lwt.Infix

type middleware = Request.t -> Request.t

type handler = Request.t -> Response.t

type uri_handler = Re.t list

type route_handler = {
  meth : Cohttp.Code.meth;
  uri_handler : uri_handler;
  handler : handler;
}

type handler_config = route_handler list

let filter_route_handler_config_by_meth meth handler_config =
  Base.List.filter handler_config (fun (route_handler : route_handler) ->
    route_handler.meth = meth
  )

let uri_separator = Re.char '/'

let uri_handler_composer uri_catcher accumulator =
  uri_separator :: uri_catcher :: accumulator

let compose_uri_handler uri_handler =
  let regexes_wit_separator = Base.List.fold_right uri_handler  ~f:uri_handler_composer ~init:[] in
  Re.seq regexes_wit_separator

let find_route_handler_config_by_route route handler_config =
  Base.List.find handler_config ~f:(fun route_handler ->
    let uri_handler = route_handler.uri_handler in 
    let composed_uri_handler = compose_uri_handler uri_handler in
    let compiled_uri_handler = Re.compile composed_uri_handler in
    Re.execp compiled_uri_handler route
  )

let server handler_config =
  let callback _conn req body =
    let uri = Request.uri req in
    let path = Uri.to_string uri in
    let meth = Request.meth req in
    let headers =  Request.headers req in
    Cohttp_lwt.Body.to_string body >>= fun body ->
    let status = Cohttp.Code.status_of_code 404 in
    Cohttp_lwt_unix.Server.respond_string ~status ~body ()
  in
  Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port 8000)) (Cohttp_lwt_unix.Server.make ~callback ())