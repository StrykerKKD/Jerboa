module Request = Request
module Response = Response
module Path = Path
module Path_mapping = Path_mapping
module Path_handler = Path_handler
module Path_handler_config = Path_handler_config
module Middleware_config = Middleware_config

let default_request_handler _ = 
  Response.create 404 ""

let start ?(port = 8000) ?(default_request_handler = default_request_handler) ?(middleware_config = []) path_handler_config =
  let server = Server.create port default_request_handler middleware_config  path_handler_config in
  Lwt_main.run server