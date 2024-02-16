open Common
open Base
open Lwt.Syntax

let save_single_file ~name_dir_fn (uri, name) =
  let* body = get_body uri in
  let dir_name = name_dir_fn body in
  let full_path = "_build/exercise_3/" ^ dir_name ^ "/" ^ name in
  let* () = Lwt_io.lines_to_file full_path (Lwt_stream.return body) in
  Lwt.return ()

let save_all_files () =
  let divider body =
    match String.length body with
    | n when n < 2000 ->
        "small"
    | n when n < 8000 ->
        "medium"
    | _ ->
        "large"
  in
  Lwt_list.iter_p (save_single_file ~name_dir_fn:divider) Uris.uris_with_names

let io_interactions () =
  let base_dir = "_build/exercise_3/" in
  let* () = make_directory_p base_dir in
  let* () = make_directory_p (base_dir ^ "small") in
  let* () = make_directory_p (base_dir ^ "medium") in
  let* () = make_directory_p (base_dir ^ "large") in
  save_all_files ()

let exec = io_interactions
