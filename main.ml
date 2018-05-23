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

type handler_response =
  | Full of status_code * body
  | Normal of status_code * body option
  | Minimal of status_code

type connect_handler = uri_parameters -> query_parameters -> (status_code * body)
type delete_handler = uri_parameters -> query_parameters -> body option -> (status_code * body option)
type get_handler = uri_parameters -> query_parameters -> (status_code * body)
type head_handler = uri_parameters -> query_parameters -> status_code
type options_handler = uri_parameters -> query_parameters -> (status_code * body)
type other_handler = string -> uri_parameters -> query_parameters -> body option -> (status_code * body option)
type patch_handler = uri_parameters -> query_parameters -> body -> status_code
type post_handler = uri_parameters -> query_parameters -> body -> (status_code * body)
type put_handler = uri_parameters -> query_parameters -> body -> status_code
type trace_handler = uri_parameters -> query_parameters -> status_code

type uri_handler = Re.t list

type method_handler = 
  | Connect_handler of uri_handler * connect_handler
  | Delete_handler of uri_handler * delete_handler
  | Get_handler of uri_handler * get_handler
  | Head_handler of uri_handler * head_handler
  | Options_handler of uri_handler * options_handler
  | Other_handler of uri_handler * other_handler
  | Patch_handler of uri_handler * patch_handler
  | Post_handler of uri_handler * post_handler
  | Put_handler of uri_handler * put_handler
  | Trace_handler of uri_handler * trace_handler

type method_handlers = 
  | Connect_handlers of (uri_handler * connect_handler) list
  | Delete_handlers of (uri_handler * delete_handler) list
  | Get_handlers of (uri_handler * get_handler) list
  | Head_handlers of (uri_handler * head_handler) list
  | Options_handlers of (uri_handler * options_handler) list
  | Other_handlers of (uri_handler * other_handler) list
  | Patch_handlers of (uri_handler * patch_handler) list
  | Post_handlers of (uri_handler * post_handler) list
  | Put_handlers of (uri_handler * put_handler) list
  | Trace_handlers of (uri_handler * trace_handler) list

type handler_config = {
  connect_handlers : (uri_handler * connect_handler) list;
  delete_handlers : (uri_handler * delete_handler) list;
  get_handlers : (uri_handler * get_handler) list;
  head_handlers : (uri_handler * head_handler) list;
  options_handlers : (uri_handler * options_handler) list;
  other_handlers: (uri_handler * other_handler) list;
  patch_handlers : (uri_handler * patch_handler) list;
  post_handlers : (uri_handler * post_handler) list;
  put_handlers : (uri_handler * put_handler) list;
  trace_handlers : (uri_handler * trace_handler) list;
}

let anytihng = Re.rep1 (Re.compl [Re.char '/'])

let uri_separator = Re.char '/'

let compose_re regexes = 
  let regexes_wit_separator = Base.List.fold_right regexes ~f:(fun regex accum -> uri_separator :: regex :: accum ) ~init:[] in
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

let find_specific_handler uri handlers =
  Base.List.find handlers (fun handler -> 
    let uri_handler, _ = handler in
    let composed_uri_handler = Re.seq uri_handler in
    let compiled_uri_handler = Re.compile composed_uri_handler in
    Re.execp compiled_uri_handler uri
  )

let find_connect_handler uri handlers = find_specific_handler
let find_delete_handler uri handlers = find_specific_handler
let find_get_handler uri handlers = find_specific_handler
let find_head_handler uri handlers = find_specific_handler
let find_options_handler uri handlers = find_specific_handler
let find_other_handler uri handlers = find_specific_handler
let find_patch_handler uri handlers = find_specific_handler
let find_post_handler uri handlers = find_specific_handler
let find_put_handler uri handlers = find_specific_handler
let find_trace_handler uri handlers = find_specific_handler

let find_method_handlers (meth:Cohttp.Code.meth) handler_config =
  match meth with
  | `CONNECT -> Connect_handlers handler_config.connect_handlers
  | `DELETE -> Delete_handlers handler_config.delete_handlers
  | `GET -> Get_handlers handler_config.get_handlers
  | `HEAD -> Head_handlers handler_config.head_handlers
  | `OPTIONS -> Options_handlers handler_config.options_handlers
  | `Other argument -> Other_handlers handler_config.other_handlers
  | `PATCH -> Patch_handlers handler_config.patch_handlers
  | `POST -> Post_handlers handler_config.post_handlers
  | `PUT -> Put_handlers handler_config.put_handlers
  | `TRACE -> Trace_handlers handler_config.trace_handlers

let find_handler uri handlers =
  match handlers with
  | Connect_handlers handlers -> find_connect_handler uri handlers
  | Delete_handlers handlers -> find_delete_handler uri handlers
  | Get_handlers handlers -> find_get_handler uri handlers
  | Head_handlers handlers -> find_head_handler uri handlers
  | Options_handlers handlers -> find_options_handler uri handlers
  | Other_handlers handlers -> find_other_handler uri handlers
  | Patch_handlers handlers -> find_patch_handler uri handlers
  | Post_handlers handlers -> find_post_handler uri handlers
  | Put_handlers handlers -> find_put_handler uri handlers
  | Trace_handlers handlers -> find_trace_handler uri handlers

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
