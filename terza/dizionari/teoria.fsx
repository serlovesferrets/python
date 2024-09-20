(*
    # I dizionari in F#, ossia i Map

    In F# esistono i dizionari, ma essendo mutabili è generalmente
    sconsigliato utilizzarli. Si tende, invece, ad utilizzare i Map.
*)

// Per creare un map, si usa Map.ofList con una lista di tuple 'a * 'b.
let report =
    [ ("italiano", 7)
      ("latino", 7)
      ("greco", 4)
      ("filosofia", 6)
      ("matematica", 10) ]
    |> Map.ofList

// Per accedere ad un valore, la sintassi è simile a Python.
// Sconsigliato, in quanto tira un'eccezione se la chiave non esiste.
// Equivale alla funzione Map.find.
let italianGrade = report["italiano"]

// Invece si utilizza "tryFind", che ritorna un'Option.
let italianGradeOpt = Map.tryFind "italiano" report

let _ =
    match italianGradeOpt with
    | Some value -> "trovato"
    | None -> "non trovato"

// Si possono modificare solamente i dizionari, non i Map.
// Per i map, si utilizza la funzione Map.add.
// Map.add serve anche per aggiornare un valore preesistente.

let report' =
    Map.add "fisica" 10 report
    |> Map.remove "greco"
    |> Map.add "matematica" 9
    |> Map.add "latino" 8

// Non esiste una funzione predefinita per unire due
// map, ma non è complesso crearla.

let mergeMaps left right =
    Map.fold (fun acc k v -> Map.add k v acc) left right

let left = Map.ofList [ (1, "a"); (2, "b") ]
let right = Map.ofList [ (2, "c"); (3, "d") ]

// merged = [ (1, "a"); (2, "c"); (3, "d") ]
let merged = mergeMaps left right

// Il dizionario vuoto è una costante: Map.empty.
let empty = Map.empty

// Per iterare su un dizionario, si utilizza Map.iter (o Map.map)
let ``voti da zero a trenta`` = Map.map (fun k v -> 3 * v) report
