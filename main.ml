open Lwt
open Cohttp
open Cohttp_lwt_unix

type uri_catch =
 | Simple of Re.t
 | Complex of string * Re.t

type uri_parameters = (string * string) list
type query_parameters = (string * string list) list
type body = string
type status_code = int

type connect_handler = uri_parameters -> query_parameters -> (status_code * body)
type delete_handler = uri_parameters -> query_parameters -> body option -> (status_code * body option)
type get_handler = uri_parameters -> query_parameters -> (status_code * body)
type head_handler = uri_parameters -> query_parameters -> status_code
type options_handler = uri_parameters -> query_parameters -> (status_code * body)
type patch_handler = uri_parameters -> query_parameters -> body -> status_code
type post_handler = uri_parameters -> query_parameters -> body -> (status_code * body)
type put_handler = uri_parameters -> query_parameters -> body -> status_code
type trace_handler = uri_parameters -> query_parameters -> status_code

type uri_handler = Re.t list

type handler_config = {
  connect_handler : (uri_handler * connect_handler) list;
  delete_handler : (uri_handler * delete_handler) list;
  get_handler : (uri_handler * get_handler) list;
  head_handler : (uri_handler * head_handler) list;
  options_handler : (uri_handler * options_handler) list;
  patch_handler : (uri_handler * patch_handler) list;
  post_handler : (uri_handler * post_handler) list;
  put_handler : (uri_handler * put_handler) list;
  trace_handler : (uri_handler * trace_handler) list;
}

let anytihng = Re.rep1 (Re.compl [Re.char '/'])

let uri_separator = Re.char '/'

let compose_re regexes = 
  let regexes_wit_separator = Base.List.fold regexes ~init:[] ~f:(fun accum regex -> uri_separator :: regex :: accum ) in
  Re.seq regexes_wit_separator

let split_uri uri =
  let raw_uri = Uri.path uri in
  let re = Re.char '/' |> Re.compile in
  Re.split re raw_uri

let capture name regex uri_part =
  let compiled_regex = Re.compile regex in
  let matches = Re.matches compiled_regex uri_part in
  let first_match = Base.List.hd matches in
  Base.Option.map first_match ~f:(fun value -> (name,value))

let handle_meth meth =
  match meth with
  | `CONNECT -> ()
  | `DELETE -> ()
  | `GET -> ()
  | `HEAD -> ()
  | `OPTIONS -> ()
  | `Other string -> ()
  | `PATCH -> ()
  | `POST -> ()
  | `PUT -> ()
  | `TRACE -> ()

let server =
  let callback _conn req body =
    let uri = Request.uri req in
    let uri_parts = split_uri uri in
    let query = Uri.query uri in
    let meth = Request.meth req in
    let headers =  Request.headers req in
    Cohttp_lwt.Body.to_string body >>= fun body ->
    Server.respond_string ~status:`OK ~body ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)

(*let server =
  let callback _conn req body =

    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt.Body.to_string >|= (fun body ->
        (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
           uri meth headers body))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)*)

(*
type nonrec status_code =                                                                                            ──┴────────────┴───────────────────┴─────────────┴───────────────┴───────────────────┴───────────────────────┴─────────────────┘
    [ `Accepted
    | `Already_reported
    | `Bad_gateway
    | `Bad_request
    | `Bandwidth_limit_exceeded
    | `Blocked_by_windows_parental_controls
    | `Checkpoint
    | `Client_closed_request
    | `Code of int
    | `Conflict
    | `Continue
    | `Created
    | `Enhance_your_calm
    | `Expectation_failed
    | `Failed_dependency
    | `Forbidden
    | `Found
    | `Gateway_timeout
    | `Gone
    | `Http_version_not_supported
    | `I_m_a_teapot
    | `Im_used
    | `Insufficient_storage
    | `Internal_server_error
    | `Length_required
    | `Locked
    | `Loop_detected
    | `Method_not_allowed
    | `Moved_permanently
    | `Multi_status
    | `Multiple_choices
    | `Network_authentication_required
    | `Network_connect_timeout_error
    | `Network_read_timeout_error
    | `No_content
    | `No_response
    | `Non_authoritative_information
    | `Not_acceptable
    | `Not_extended
    | `Not_found
    | `Not_implemented
    | `Not_modified
    | `OK
    | `Partial_content
    | `Payment_required
    | `Precondition_failed
    | `Precondition_required
    | `Processing
    | `Proxy_authentication_required
    | `Request_entity_too_large
    | `Request_header_fields_too_large
    | `Request_timeout
    | `Request_uri_too_long
    | `Requested_range_not_satisfiable
    | `Reset_content
    | `Resume_incomplete
    | `Retry_with
    | `See_other
    | `Service_unavailable
    | `Switch_proxy
    | `Switching_protocols
    | `Temporary_redirect
    | `Too_many_requests
    | `Unauthorized
    | `Unprocessable_entity
    | `Unsupported_media_type
    | `Upgrade_required
    | `Use_proxy
    | `Variant_also_negotiates
    | `Wrong_exchange_server ]
*)
