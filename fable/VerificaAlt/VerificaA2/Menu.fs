module Menu

open System

type MenuFunction = unit -> unit

type MenuItem =
    { Id: int
      Description: string
      Function: MenuFunction }

type Menu = { Items: MenuItem list; ExitOpt: int }

exception NotMenuItemException of string

let newLine () = printf $"{Environment.NewLine}"

let from (items: list<string * MenuFunction>) : Menu =
    let menuItems =
        items
        |> List.mapi (fun i (desc, fn) ->
            { Id = i + 1
              Description = desc
              Function = fn })

    let itemsWithExit =
        menuItems
        |> List.rev
        |> (fun items ->
            { Id = (List.length menuItems) + 1
              Description = "Exit"
              Function = (fun _ -> ()) }
            :: items)
        |> List.rev

    { Items = itemsWithExit
      ExitOpt = List.length itemsWithExit }

let printOpts (menu: Menu) =
    for item in menu.Items do
        printfn $"{item.Id}. {item.Description}"

let run (menu: Menu) =
    let mutable shouldLoop = true

    newLine ()
    printOpts menu
    printf "Quale opzione vuoi? "

    while shouldLoop do
        let ok, choice =
            try
                let value = Console.ReadLine() |> int

                if value < 1 || value > menu.ExitOpt then
                    raise (NotMenuItemException("Non un'opzione valida! Riprova."))
                else
                    ()

                true, value
            with
            | :? FormatException ->
                printfn "Non un numero! Riprova."
                false, -1
            | :? ArgumentNullException ->
                printfn "Devi inserire qualcosa! Riprova."
                false, -1
            | NotMenuItemException(message) ->
                printfn $"{message}"
                false, -1

        if ok then
            let func =
                menu.Items
                |> List.where (fun item -> item.Id = choice)
                |> List.head
                |> _.Function

            newLine ()
            do func ()

        shouldLoop <- not ok || choice <> menu.ExitOpt

        if shouldLoop then
            newLine ()
            printOpts menu
            printf "Quale opzione vuoi? "
