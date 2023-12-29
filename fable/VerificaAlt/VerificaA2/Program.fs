open System

(*
## Esercizio n.2 -  Vendite per reparto

Nella tupla_vendite sono memorizzate le  vendite. 
Per ogni vendita si riporta: reparto, categoria, prodotto, tipologia 
di pagamento e importo. Utilizzare le funzioni predefinite sum,min,max 
per le liste.

Richieste:
1. Definire una funzione chiamata media_globale che accetti come 
   parametro la tupla delle vendite e restituisca l'importo medio delle 
   vendite indipendentemente dal tipo di pagamento.
2. Definire una funzione chiamata media che accetti come parametri la 
   tupla delle vendite, la categoria e la tipologia di pagamento (contanti
   o carta di credito) e restituisca l'importo medio delle vendite.
3. Definire una funzione chiamata venditaMax che restituisca una tupla 
   contenente la vendita massima e il prodotto (o i prodotti), 
   indipendentemente dalla tipologia di pagamento.
4. Definire una funzione chiamata venditaMin che restituisca una tupla 
   contenente la vendita inferiore registrata nel reparto RepartoA.
5. Definire una funzione chiamata venditaPer che restituisca una tupla 
   contenente la percentuale delle vendite per reparto rispetto al totale.
6. Prevedere un menu di scelta.
*)


type Pagamento =
    | Contanti of float
    | Carta of float
    | ``Non disponibile``

    member self.Value =
        match self with
        | Contanti v -> v
        | Carta v -> v
        | ``Non disponibile`` -> failwith "nessun valore"


    override self.ToString() =
        match self with
        | Contanti _ -> sprintf "contanti"
        | Carta _ -> sprintf "carta"
        | ``Non disponibile`` -> sprintf "non disponibile"


type Reparto = { Sezione: string; Categoria: string }
type Prodotto = { Nome: string; Pagamento: Pagamento }

type Vendita =
    { Reparto: Reparto; Prodotto: Prodotto }

let getValue (vendita: Vendita) =
    match vendita.Prodotto.Pagamento with
    | Contanti v
    | Carta v -> v
    | ``Non disponibile`` -> failwith "non disponibile"

let getValueDefault defaultValue (vendita: Vendita) =
    match vendita.Prodotto.Pagamento with
    | Contanti v
    | Carta v -> v
    | ``Non disponibile`` -> defaultValue


let tuplaVendite =
    [ { Reparto =
          { Sezione = "A"
            Categoria = "Informatica" }
        Prodotto =
          { Nome = "Prodotto A"
            Pagamento = Contanti 1000 } }
      { Reparto =
          { Sezione = "A"
            Categoria = "Informatica" }
        Prodotto =
          { Nome = "Prodotto B"
            Pagamento = Contanti 1500 } }
      { Reparto =
          { Sezione = "A"
            Categoria = "Informatica" }
        Prodotto =
          { Nome = "Prodotto C"
            Pagamento = Carta 1200.0 } }

      { Reparto =
          { Sezione = "A"
            Categoria = "Informatica" }
        Prodotto =
          { Nome = "Prodotto D"
            Pagamento = Contanti 200.0 } }

      { Reparto =
          { Sezione = "A"
            Categoria = "Informatica" }
        Prodotto =
          { Nome = "Prodotto E"
            Pagamento = Contanti 800.0 } }

      { Reparto =
          { Sezione = "A"
            Categoria = "Informatica" }
        Prodotto =
          { Nome = "Prodotto F"
            Pagamento = ``Non disponibile`` } }

      { Reparto =
          { Sezione = "B"
            Categoria = "Elettronica" }
        Prodotto =
          { Nome = "Prodotto A"
            Pagamento = Contanti 1500.0 } }

      { Reparto =
          { Sezione = "B"
            Categoria = "Elettronica" }
        Prodotto =
          { Nome = "Prodotto B"
            Pagamento = Carta 900.0 } } ]

// Media totale
// Media con categoria e tipo pagamento
// Vendita max/min e prodotti associati
// Pagamento -> Percentuale

let totale, conto =
    tuplaVendite
    |> List.fold
        (fun (acc, count) vendita ->
            match vendita.Prodotto.Pagamento with
            | Contanti v -> acc + v, count + 1.0
            | Carta v -> acc + v, count + 1.0
            | ``Non disponibile`` -> acc, count)
        (0.0, 0.0)

// Prima richiesta
let mediaGlobale = totale / conto

let menuMediaGlobale () =
    printfn $"La media globale è %.2f{mediaGlobale}."

// Seconda richiesta
let media categoria pagamento =
    tuplaVendite
    |> List.fold
        (fun acc vendita ->
            if
                vendita.Prodotto.Pagamento.ToString() = pagamento
                && vendita.Reparto.Categoria.ToLower() = categoria
            then
                vendita.Prodotto.Pagamento.Value :: acc
            else
                acc)
        []
    |> (fun xs -> if List.isEmpty xs then None else Some xs)
    |> Option.map (List.average)


let menuMedia () =
    printf "Quale categoria vuoi? "
    let categoriaRichiesta = (Console.ReadLine()).ToLower()

    printf "Che tipo di pagamento vuoi? "
    let pagamento = Console.ReadLine()
    let mediaUtente = media categoriaRichiesta pagamento

    printfn $"La media è {mediaUtente:F2}."

// Terza e quarta richiesta
let venditaInBaseA determinatore mempty =
    let valore = tuplaVendite |> determinatore (getValueDefault mempty)

    tuplaVendite
    |> List.fold
        (fun acc vendita ->
            match vendita.Prodotto.Pagamento with
            | Carta _
            | Contanti _ -> vendita :: acc
            | ``Non disponibile`` -> acc)
        []
    |> List.filter (fun vendita -> getValue vendita = getValue valore)

let venditaMax = venditaInBaseA List.maxBy Int32.MinValue
let venditaMin = venditaInBaseA List.minBy Int32.MaxValue

let menuVenditaMax () =
    printfn $"La vendita massima è {venditaMax:F2}."

let menuVenditaMin () =
    printfn $"La vendita minima è {venditaMin:F2}."

// Quinta richiesta
type DatoMedia =
    { Reparto: string
      Percentuale: float }

    override self.ToString() =
        sprintf "%s: %.2f" self.Reparto self.Percentuale

let mediaPerNotUnique =
    tuplaVendite
    |> List.map (fun vendita ->
        let pagamento = getValueDefault 0 vendita
        let percentuale = pagamento / totale * 100.0

        { Reparto = vendita.Reparto.Categoria
          Percentuale = percentuale })

let mediaPer =
    mediaPerNotUnique
    |> List.fold
        (fun acc value ->
            if Map.containsKey value.Reparto acc then
                let key = Map.find value.Reparto acc
                Map.add value.Reparto (key + value.Percentuale) acc
            else
                Map.add value.Reparto value.Percentuale acc)
        Map.empty

let menuMediaPer () =
    mediaPer |> Map.iter (fun k v -> printfn "%s: %.2f%% " k v)


Menu.from
    [ "Media globale", menuMediaGlobale
      "Media per categoria e pagamento", menuMedia
      "Vendita massima", menuVenditaMax
      "Vendita minima", menuVenditaMin
      "Percentuale vendite per reparto", menuMediaPer ]
|> Menu.run

