open Base
open Lwt.Syntax
open Working_with_files

type exercise = N1 | N2 | N3

let async_main =
  let* () = Common.make_directory_p "_build" in
  let exercise_num =
    let args = Sys.get_argv () in
    if Array.length args < 2 then N1
    else
      let arg = Array.get args 1 in
      match arg with "2" -> N2 | "3" -> N3 | _ -> N1
  in
  let exercise =
    match exercise_num with
    | N1 ->
        Exercise_1.exec
    | N2 ->
        Exercise_2.exec
    | N3 ->
        Exercise_3.exec
  in
  exercise ()

let () = Lwt_main.run async_main
