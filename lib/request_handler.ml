(** [Request_handler.t] is the type definition of the handler, which will handle the request and produce a response.*)
type t = Request.t -> Response.t