let uris_with_names =
  [ ("http://elexpo.altervista.org/r.txt", "file1.txt")
  ; ("https://jsonplaceholder.typicode.com/users", "file2.json")
  ; ("https://jsonplaceholder.typicode.com/posts", "file3.json") ]
  |> List.map (fun (uri, name) -> (Uri.of_string uri, name))
