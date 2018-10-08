module Request = Request
module Response = Response
module Path = Path
module Path_mapping = Path_mapping
module Path_handler = Path_handler

let default_request_handler _ = 
  Response.create 404 ""

let start ?(port = 8000) ?(default_request_handler = default_request_handler) ?(middleware_config = []) path_handler_config =
  let server = Server.create port default_request_handler middleware_config  path_handler_config in
  Lwt_main.run server