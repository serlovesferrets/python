open Base
open Async
open Cohttp_async
open Cohttp
open Websites

let check_async uri =
  let uri_str = Uri.to_string uri in
  let%bind res =
    try_with (fun () -> Client.get uri)
    >>| function
    | Ok (resp, _) -> return (Some resp.status)
    | _ -> return None
  in
  res
  >>| function
  | None -> "Not found " ^ uri_str
  | Some code ->
    let str = Code.string_of_status code in
    (match code with
     | `OK | `Found | `Permanent_redirect | `Moved_permanently ->
       "Ok, code " ^ str ^ " for " ^ uri_str
     | _ -> "Error, code " ^ str ^ " for " ^ uri_str)
;;

let rec iter_and_print_sync fn = function
  | [] -> ()
  | x :: xs ->
    upon (fn x)
    @@ fun res ->
    Stdio.print_endline res;
    iter_and_print_sync fn xs
;;

let print_results_sync () = iter_and_print_sync check_async websites
let results_async () = List.(websites >>| check_async)

let print_results_async () =
  List.iter (results_async ()) ~f:(fun result -> upon result print_endline)
;;
