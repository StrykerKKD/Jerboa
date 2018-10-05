let create port default_request_handler middleware_config path_handler_config =
  let open Lwt.Infix in
  let callback _conn req body =
    Request.create req body >>= fun request ->
    let request = Middleware_config.apply_middlewares middleware_config request in
    let meth = request.meth in
    let path = request.path in
    let path_handler = Path_handler_config.find_path_handler path_handler_config meth path in
    let response = Path_handler.apply_path_handler path_handler request default_request_handler in
    let status, body = Response.convert_to_cohttp_response response in
    Cohttp_lwt_unix.Server.respond_string ~status ~body ()
  in
  Cohttp_lwt_unix.Server.create 
    ~mode:(`TCP (`Port port)) (Cohttp_lwt_unix.Server.make ~callback ())