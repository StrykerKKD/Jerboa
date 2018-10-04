let default_request_handler _ = 
  Response.create 404 ""

let create_server port default_request_handler middleware_config path_handler_config =
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

let start ?(port = 8000) ?(default_request_handler = default_request_handler) ?(middleware_config = []) path_handler_config =
  let server = create_server port default_request_handler middleware_config  path_handler_config in
  Lwt_main.run server

let my_middleware request = 
  let open Request in
  if request.path = "/" then 
    {request with path = "/hello/world"}
  else
    request

let my_middleware_config = [my_middleware]

let my_path_handler = Path_handler.create `GET [Path.part "hello"; Path.var "name"] (fun request ->
  let open Request in
  let found_path_parameter = Base.List.Assoc.find request.path_parameter ~equal:(=) "name" in
  Response.create 200 ("Hello " ^ (Base.Option.value found_path_parameter ~default:"not found")) 
)

let my_path_handler_config = [my_path_handler]

let () = ignore (start ~middleware_config:my_middleware_config my_path_handler_config)