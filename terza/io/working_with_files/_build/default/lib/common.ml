open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open Lwt.Syntax

let make_directory name = Lwt_unix.mkdir name 0o755

let make_directory_p name =
  let* exists = Lwt_unix.file_exists name in
  if exists then Lwt.return () else make_directory name

let get_body uri =
  let* resp, body = Client.get uri in
  let* body_string = Body.to_string body in
  let status_code = Response.status resp in
  let contents =
    match status_code with
    | `OK ->
        Lwt.return body_string
    | other ->
        let other_string = Code.string_of_status other in
        let msg = Printf.sprintf "error code: %s" other_string in
        Lwt.return msg
  in
  contents
