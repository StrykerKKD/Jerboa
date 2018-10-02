type middleware = Request.t -> Request.t

type request_handler = Request.t -> Response.t

type meth = Cohttp.Code.meth

type path_handler = {
  meth: meth;
  path_mapping: Path_mapping.t;
  request_handler: request_handler;
}

type path_handler_config = path_handler list

let filter_path_handler_config_by_meth meth path_handler_config =
  Base.List.filter path_handler_config ~f:(fun (path_handler) ->
      path_handler.meth = meth
    )

let find_path_handler path path_handler_config =
  Base.List.find path_handler_config ~f:(fun path_handler ->
      let path_mapping = path_handler.path_mapping in 
      let composed_path_regex = Path_mapping.compose_path_regex path_mapping in
      let compiled_path_regex = Re.compile composed_path_regex in
      Re.execp compiled_path_regex path
    )

let handle_path_handler path_handler request =
  let path_mapping = path_handler.path_mapping in
  let request = Request.add_path_parameters request path_mapping in
  path_handler.request_handler request

let apply_path_handler path_handler request =
  match path_handler with
  | Some path_handler -> handle_path_handler path_handler request
  | None -> Response.create 200 "Default"

let server path_handler_config =
  let open Lwt.Infix in
  let callback _conn req body =
    Request.create req body >>= fun request ->
    let meth = request.meth in
    let path = request.path in
    let filtered_handler_config = filter_path_handler_config_by_meth meth path_handler_config in
    let path_handler = find_path_handler path filtered_handler_config in
    let response = apply_path_handler path_handler request in
    let status, body = Response.convert_to_cohttp_response response in
    Cohttp_lwt_unix.Server.respond_string ~status ~body ()
  in
  Cohttp_lwt_unix.Server.create 
    ~mode:(`TCP (`Port 8000)) (Cohttp_lwt_unix.Server.make ~callback ())

let my_path_handler = {
  meth = `GET;
  path_mapping = [Path.part "hello"; Path.var "name"];
  request_handler = (fun request -> 
    let open Request in
    let found_path_parameter = Base.List.Assoc.find request.path_parameter ~equal:(=) "name" in
    Response.create 200 ("Hello " ^ (Base.Option.value found_path_parameter ~default:"not found")) );
}

let my_path_handler_config = [my_path_handler]

let () = ignore (Lwt_main.run (server my_path_handler_config))