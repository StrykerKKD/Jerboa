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

type method_handler = 
  | Connect_handler of connect_handler
  | Delete_handler of delete_handler 
  | Get_handler of get_handler
  | Head_handler of head_handler
  | Options_handler of options_handler
  | Other_handler of other_handler
  | Patch_handler of patch_handler
  | Post_handler of post_handler 
  | Put_handler of put_handler
  | Trace_handler of trace_handler

type uri_catcher =
  | Simple_catch of Re.t
  | Parameter_catch of string * Re.t

type uri_handler = uri_catcher list

type connect_handler_config = uri_handler * connect_handler
type delete_handler_config = uri_handler * delete_handler
type get_handler_config = uri_handler * get_handler
type head_handler_config = uri_handler * head_handler
type options_handler_config = uri_handler * options_handler
type other_handler_config = uri_handler * other_handler
type patch_handler_config = uri_handler * patch_handler
type post_handler_config = uri_handler * post_handler
type put_handler_config = uri_handler * put_handler
type trace_handler_config = uri_handler * trace_handler

type method_handler_config = 
  | Connect_handler_config of connect_handler_config
  | Delete_handler_config of delete_handler_config
  | Get_handler_config of get_handler_config
  | Head_handler_config of head_handler_config
  | Options_handler_config of options_handler_config
  | Other_handler_config of other_handler_config
  | Patch_handler_config of patch_handler_config
  | Post_handler_config of post_handler_config
  | Put_handler_config of put_handler_config
  | Trace_handler_config of trace_handler_config

type method_handler_configs = 
  | Connect_handler_configs of connect_handler_config list
  | Delete_handler_configs of delete_handler_config list
  | Get_handler_configs of get_handler_config list
  | Head_handler_configs of head_handler_config list
  | Options_handler_configs of options_handler_config list
  | Other_handler_configs of other_handler_config list
  | Patch_handler_configs of patch_handler_config list
  | Post_handler_configs of post_handler_config list
  | Put_handler_configs of put_handler_config list
  | Trace_handler_configs of trace_handler_config list

type handler_config = {
  connect_handlers : connect_handler_config list;
  delete_handlers : delete_handler_config list;
  get_handlers : get_handler_config list;
  head_handlers : head_handler_config list;
  options_handlers : options_handler_config list;
  other_handlers: other_handler_config list;
  patch_handlers : patch_handler_config list;
  post_handlers : post_handler_config list;
  put_handlers : put_handler_config list;
  trace_handlers : trace_handler_config list;
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

let find_method_handler_config uri handler_config =
  Base.List.find_exn handler_config ~f:(fun (uri_handler, _) -> 
    let composed_uri_handler = compose_uri_handler uri_handler in
    let compiled_uri_handler = Re.compile composed_uri_handler in
    Re.execp compiled_uri_handler uri
  )

let get_method_handler_configs meth handler_config =
  match meth with
  | `CONNECT -> Connect_handler_configs handler_config.connect_handlers
  | `DELETE -> Delete_handler_configs handler_config.delete_handlers
  | `GET -> Get_handler_configs handler_config.get_handlers
  | `HEAD -> Head_handler_configs handler_config.head_handlers
  | `OPTIONS -> Options_handler_configs handler_config.options_handlers
  | `Other argument -> Other_handler_configs handler_config.other_handlers
  | `PATCH -> Patch_handler_configs handler_config.patch_handlers
  | `POST -> Post_handler_configs handler_config.post_handlers
  | `PUT -> Put_handler_configs handler_config.put_handlers
  | `TRACE -> Trace_handler_configs handler_config.trace_handlers

let get_method_handler_config uri method_handler_configs =
  match method_handler_configs with
  | Connect_handler_configs connect_handler_configs -> Connect_handler_config (find_method_handler_config uri connect_handler_configs)
  | Delete_handler_configs delete_handler_configs -> Delete_handler_config (find_method_handler_config uri delete_handler_configs)
  | Get_handler_configs get_handler_configs -> Get_handler_config (find_method_handler_config uri get_handler_configs)
  | Head_handler_configs head_handler_configs -> Head_handler_config (find_method_handler_config uri head_handler_configs)
  | Options_handler_configs options_handler_configs -> Options_handler_config (find_method_handler_config uri options_handler_configs)
  | Other_handler_configs other_handler_configs -> Other_handler_config (find_method_handler_config uri other_handler_configs)
  | Patch_handler_configs patch_handler_configs -> Patch_handler_config (find_method_handler_config uri patch_handler_configs)
  | Post_handler_configs post_handler_configs -> Post_handler_config (find_method_handler_config uri post_handler_configs)
  | Put_handler_configs put_handler_configs -> Put_handler_config (find_method_handler_config uri put_handler_configs)
  | Trace_handler_configs trace_handler_configs -> Trace_handler_config (find_method_handler_config uri trace_handler_configs)

let get_uri_handler method_handler_config =
  match method_handler_config with
  | Connect_handler_config (uri_handler, _) -> uri_handler
  | Delete_handler_config (uri_handler, _) -> uri_handler
  | Get_handler_config (uri_handler, _) -> uri_handler
  | Head_handler_config (uri_handler, _) -> uri_handler
  | Options_handler_config (uri_handler, _) -> uri_handler
  | Other_handler_config (uri_handler, _) -> uri_handler
  | Patch_handler_config (uri_handler, _) -> uri_handler
  | Post_handler_config (uri_handler, _) -> uri_handler
  | Put_handler_config (uri_handler, _) -> uri_handler
  | Trace_handler_config (uri_handler, _) -> uri_handler

let get_method_handler method_handler_config =
  match method_handler_config with
  | Connect_handler_config (_, connect_handler) -> Connect_handler connect_handler
  | Delete_handler_config (_, delete_handler) -> Delete_handler delete_handler
  | Get_handler_config (_, get_handler) -> Get_handler get_handler
  | Head_handler_config (_, head_handler) -> Head_handler head_handler
  | Options_handler_config (_, options_handler) -> Options_handler options_handler
  | Other_handler_config (_, other_handler) -> Other_handler other_handler
  | Patch_handler_config (_, patch_handler) -> Patch_handler patch_handler
  | Post_handler_config (_, post_handler) -> Post_handler post_handler
  | Put_handler_config (_, put_handler) -> Put_handler put_handler
  | Trace_handler_config (_, trace_handler) -> Trace_handler trace_handler

let server handler_config =
  let callback _conn req body =
    let uri = Request.uri req in
    let path = Uri.to_string uri in
    let meth = Request.meth req in
    let method_handler_configs = get_method_handler_configs meth handler_config in
    let method_handler_config = get_method_handler_config path method_handler_configs in
    let uri_handler = get_uri_handler method_handler_config in
    let uri_parts = split_uri uri in
    let parameters = capture_parameters uri_handler uri_parts in
    let method_handler = get_method_handler method_handler_config in
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
