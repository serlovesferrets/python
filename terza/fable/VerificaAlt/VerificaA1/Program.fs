open System

(*
Supponiamo di avere una tupla contenente i dati pluviometrici mensili  dei capoluoghi di provincia lombardi.
Ogni voce nella tupla è strutturata nel seguente modo:
*)

let misurazioni =
    [ ("Milano",
       [ "Gennaio", None
         "Febbraio", Some 0.0
         "Marzo", Some 9.0
         "Aprile", Some 12.0
         "Maggio", Some 16.0 ])
      ("Brescia",
       [ "Gennaio", Some 9.0
         "Febbraio", None
         "Marzo", Some 0.0
         "Aprile", None
         "Maggio", Some 7.0 ])
      ("Napoli",
       [ "Gennaio", Some 15.0
         "Febbraio", Some 16.0
         "Marzo", Some 7.0
         "Aprile", None
         "Maggio", None ]) ]

(*
Dove :
    * "città" rappresenta il nome del capoluogo di provincia
    * "meseX" indica il mese di riferimento
    * "valoreX" è il quantitativo di pioggia caduta in un mese in mm.
In mancanza di pioggia memorizzare il valore 0. 
Si potrebbe verificare la situazione N/D (indica valore non disponibile)
*)

(*
Per analizzare i dati sulle precipitazioni di una città definisci una funzione 
che riceva come parametro il nome della città e restituisca una tupla con 
questa struttura (media,(valoreMax,meseMax),(valoreMin,meseMin)).

Dove:
    * media: valore medio calcolato solo per i mesi disponibili*
    * meseMax e meseMin: i mesi in cui si sono registrate maggiori 
      precipitazioni e minori precipitazioni.
    * valoreMax e valoreMin: i rispettivi valori rilevati

**N.B*:** Nel calcolo della media non includere i mesi per i quale 
non risulta disponibile il valore.

Ad esempio: ("roma", [("gennaio", "N/D"), ("febbraio", 20), ... , ("Dicembre",23)])

La media andrà calcolata su 11 mesi e non su 12.

**N.B:**Prevedere la possibilità che la tupla ritornata sia vuota
*)


let calcolaDati nome =
    let (>>=) a b = Option.map b a
    let isNone, isSome, get = Option.isNone, Option.isSome, Option.get

    let dati =
        misurazioni
        |> List.tryFind (fst >> (=) nome)
        >>= snd
        >>= List.filter (snd >> isSome)
        >>= List.filter (snd >> get >> (<>) 0.0)
        >>= List.map (fun (nome, misura) -> nome, get misura)

    let media = dati >>= List.averageBy snd
    let meseMax = dati >>= List.maxBy snd
    let meseMin = dati >>= List.minBy snd

    if isNone media || isNone meseMax || isNone meseMin then
        None
    else
        Some(get media, get meseMax, get meseMin)

printf "Di quale città vuoi i dati? "
let nome = Console.ReadLine()
let dati = calcolaDati nome

match dati with
| None -> printfn "Niente!"
| Some(media, (meseMax, valMax), (meseMin, valMin)) ->
    printfn
        """
Media: %.2f
Mese con valore massimo: %s, %.2f
Mese con valore minimo: %s, %.2f
"""
        media
        meseMax
        valMax
        meseMin
        valMin
