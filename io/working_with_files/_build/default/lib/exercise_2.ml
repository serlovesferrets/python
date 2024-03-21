open Common
open Base
open Stdio
open Lwt.Syntax
open Lwt.Infix

let save_single_file ~name_dir_fn (uri, name) =
  let* body = get_body uri in
  let dir_name = name_dir_fn body in
  let full_path = "_build/exercise_2/" ^ dir_name ^ "/" ^ name in
  let* () = Lwt_io.lines_to_file full_path (Lwt_stream.return body) in
  Lwt.return ()

let save_all_files ~word_to_find ~if_word_name ~if_not_word_name =
  let divider body =
    let re = Re2.create_exn word_to_find in
    if Re2.matches re body then if_word_name else if_not_word_name
  in
  Lwt_list.iter_p (save_single_file ~name_dir_fn:divider) Uris.uris_with_names

let io_interactions () =
  let or_default default str = if String.is_empty str then default else str in
  let* () = make_directory_p "_build/exercise_2" in
  print_endline "Che parola vuoi trovare?" ;
  let* word_to_find = Lwt_io.read_line Lwt_io.stdin >|= or_default " " in
  print_endline "In che cartella, se il file la contiene?" ;
  let* if_word_name = Lwt_io.read_line Lwt_io.stdin >|= or_default "ok" in
  print_endline "Altrimenti, in che cartella?" ;
  let* if_not_word_name = Lwt_io.read_line Lwt_io.stdin >|= or_default "ko" in
  let* () = make_directory_p ("_build/exercise_2/" ^ if_not_word_name) in
  let* () = make_directory_p ("_build/exercise_2/" ^ if_word_name) in
  let* () = save_all_files ~word_to_find ~if_word_name ~if_not_word_name in
  Lwt.return ()

let exec = io_interactions
