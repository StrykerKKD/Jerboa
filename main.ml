open Lwt
open Cohttp
open Cohttp_lwt_unix

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

type handler =
  | Connect_handler of connect_handler
  | Delete_handler of delete_handler
  | Get_handler of get_handler
  | Head_handler  of head_handler 
  | Options_handler of options_handler
  | Other_handler of other_handler
  | Patch_handler of patch_handler
  | Post_handler  of post_handler 
  | Put_handler of put_handler
  | Trace_handler of trace_handler

type uri_catcher =
  | Simple_catch of Re.t
  | Parameter_catch of string * Re.t

type uri_handler = uri_catcher list

type method_handler = uri_handler * handler

type method_handlers = method_handler list

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

let uri_catch_composer uri_catcher accumulator = 
  match uri_catcher with
  | Simple_catch uri_catch -> uri_separator :: uri_catch :: accumulator
  | Parameter_catch (_, uri_catch) -> uri_separator :: uri_catch :: accumulator

let compose_uri_handler uri_handler =
  let regexes_wit_separator = Base.List.fold_right uri_handler ~f:uri_catch_composer ~init:[] in
  Re.seq regexes_wit_separator

let split_uri uri =
  let raw_uri = Uri.path uri in
  let re = Re.char '/' |> Re.compile in
  Re.split re raw_uri

let capture parameter uri_catch uri_part =
  let compiled_uri_catch = Re.compile uri_catch in
  let matches = Re.matches compiled_uri_catch uri_part in
  (parameter, Base.List.hd matches)

let capture_parameter accumulator uri_catcher uri_part =
  match uri_catcher with
  | Simple_catch uri_catch -> accumulator
  | Parameter_catch (parameter, uri_catch) -> (capture parameter uri_catch uri_part) :: accumulator

let capture_parameters uri_handler uri_parts =
  Base.List.fold2 uri_handler uri_parts ~init:[] ~f:capture_parameter

let collect_uri_handlers handler_config =
  Base.List.map handler_config ~f:(fun (uri_handler, _) -> uri_handler)

let find_specific_handler uri handler_config =
  let uri_handlers = collect_uri_handlers handler_config in
  Base.List.find uri_handlers (fun uri_handler ->
    let composed_uri_handler = compose_uri_handler uri_handler in
    let compiled_uri_handler = Re.compile composed_uri_handler in
    Re.execp compiled_uri_handler uri
  )
  (*Base.List.find handler_config (fun (uri_handler, _) -> 
    let composed_uri_handler = compose_uri_handler uri_handler in
    let compiled_uri_handler = Re.compile composed_uri_handler in
    Re.execp compiled_uri_handler uri
  )*)

let find_connect_handler uri handler_config = find_specific_handler uri handler_config
let find_delete_handler uri handler_config = find_specific_handler uri handler_config
let find_get_handler uri handler_config  = find_specific_handler uri handler_config
let find_head_handler uri handler_config = find_specific_handler uri handler_config
let find_options_handler uri handler_config = find_specific_handler uri handler_config
let find_other_handler uri handler_config = find_specific_handler uri handler_config
let find_patch_handler uri handler_config = find_specific_handler uri handler_config
let find_post_handler uri handler_config = find_specific_handler uri handler_config
let find_put_handler uri handler_config = find_specific_handler uri handler_config
let find_trace_handler uri handler_config = find_specific_handler uri handler_config

let find_method_handlers meth handler_config =
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

let find_handler uri method_handlers =
  match method_handlers with
  | Connect_handlers handler_config -> find_connect_handler uri handler_config
  | Delete_handlers handler_config -> find_delete_handler uri handler_config
  | Get_handlers handler_config -> find_get_handler uri handler_config
  | Head_handlers handler_config -> find_head_handler uri handler_config
  | Options_handlers handler_config -> find_options_handler uri handler_config
  | Other_handlers handler_config -> find_other_handler uri handler_config
  | Patch_handlers handler_config -> find_patch_handler uri handler_config
  | Post_handlers handler_config -> find_post_handler uri handler_config
  | Put_handlers handler_config -> find_put_handler uri handler_config
  | Trace_handlers handler_config -> find_trace_handler uri handler_config

let server handler_config =
  let callback _conn req body =
    let uri = Request.uri req in
    let path = Uri.to_string uri in
    let meth = Request.meth req in
    let method_handlers = find_method_handlers meth handler_config in
    let uri_handler = find_handler path method_handlers in
    let uri_parts = split_uri uri in
    let parameters = capture_parameters (Base.Option.value_exn uri_handler) uri_parts in
    let query = Uri.query uri in
    let headers =  Request.headers req in
    Cohttp_lwt.Body.to_string body >>= fun body ->
    Server.respond_string ~status:`OK ~body ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

(*let () = ignore (Lwt_main.run server)*)

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
