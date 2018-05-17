open Lwt
open Cohttp
open Cohttp_lwt_unix

type uri_catch =
 | Simple of Re.t
 | Complex of string * Re.t

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
  | `GET -> ()
  | `POST -> ()

(*
    let the_uri = Request.uri req in
    let uri_parts = split_uri the_uri in
*)

let server =
  let callback _conn req body =
    let uri = Request.uri req in
    let uri_parts = split_uri uri in
    let meth = Request.meth req in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt.Body.to_string >|= (fun body ->
        (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
           uri meth headers body))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
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
