open Common
open Lwt.Syntax

let save_single_file (uri, name) =
  let* body = get_body uri in
  Lwt_io.lines_to_file ("./_build/exercise_1/" ^ name) (Lwt_stream.return body)

let save_all_files () =
  let* () = make_directory_p "./_build/exercise_1" in
  Lwt_list.iter_p save_single_file Uris.uris_with_names

let exec = save_all_files
