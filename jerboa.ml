type middleware = Request.t -> Request.t

type handler = Request.t -> Response.t

type path_handler = Path.t list

type meth = Cohttp.Code.meth

type route_handler = {
  meth: meth;
  path_handler: path_handler;
  handler: handler;
}

type handler_config = route_handler list

let filter_handler_config_by_meth meth handler_config =
  Base.List.filter handler_config ~f:(fun (route_handler : route_handler) ->
      route_handler.meth = meth
    )

let path_handler_composer path_catcher accumulator =
  Path.separator :: path_catcher :: accumulator

let compose_path_handler path_handler =
  let path_wit_separator = Base.List.fold_right path_handler ~f:path_handler_composer ~init:[] in
  let regex_wit_separator = Base.List.map path_wit_separator ~f:(fun path -> path.regex) in
  let beginning_regex = [Re.bol] in
  let ending_regex = [Re.opt (Re.char '/'); Re.eol] in
  let final_regex = Base.List.concat [beginning_regex; regex_wit_separator; ending_regex] in
  Re.seq final_regex

let find_route_handler route handler_config =
  Base.List.find handler_config ~f:(fun route_handler ->
      let path_handler = route_handler.path_handler in 
      let composed_path_handler = compose_path_handler path_handler in
      let compiled_path_handler = Re.compile composed_path_handler in
      Re.execp compiled_path_handler route
    )

let handle_route_handler route_handler request =
  let path_handler = route_handler.path_handler in
  let request = Request.add_path_parameters request path_handler in
  route_handler.handler request

let apply_route_handler route_handler request =
  match route_handler with
  | Some route_handler -> handle_route_handler route_handler request
  | None -> Response.create 200 "Default"

let server handler_config =
  let open Lwt.Infix in
  let callback _conn req body =
    Request.create req body >>= fun request ->
    let meth = request.meth in
    let path = request.path in
    let filtered_handler_config = filter_handler_config_by_meth meth handler_config in
    let route_handler = find_route_handler path filtered_handler_config in
    let response = apply_route_handler route_handler request in
    let status, body = Response.convert_to_cohttp_response response in
    Cohttp_lwt_unix.Server.respond_string ~status ~body ()
  in
  Cohttp_lwt_unix.Server.create 
    ~mode:(`TCP (`Port 8000)) (Cohttp_lwt_unix.Server.make ~callback ())

let my_route_handler = {
  meth = `GET;
  path_handler = [Path.part "hello"; Path.var "name"];
  handler = (fun request -> 
    let open Request in
    let found_name = Base.List.Assoc.find request.path_parameter ~equal:(=) "name" in
    Response.create 200 ("Hello " ^ (Base.Option.value found_name ~default:"not found")) );
}

let my_handler_config = [my_route_handler]

let () = ignore (Lwt_main.run (server my_handler_config))