open Base
open Async
open Http_requests
open Site_checker

let () =
  let args = Sys.get_argv () in
  let should_be_sync = if Array.length args < 2 then None else Some (Array.get args 1) in
  match should_be_sync with
  | None ->
    Stdio.print_endline "Pass an option! [sync/async]";
    Shutdown.shutdown 1
  | Some opt ->
    (match opt with
     (* Nota: asincrono tramite scheduler. 
      * Per usare thread, si utilizzerebbe il modulo Domain.
      * Troppo complesso per via di Cohttp che non supporta
      * Richieste sincrone. 
      *)
     | "async" -> print_results_async ()
     | "sync" -> print_results_sync ()
     | _ ->
       Stdio.print_endline "Invalid option! [sync/async]";
       Shutdown.shutdown 1)
;;

let () = Nothing.unreachable_code (Scheduler.go ())
