(* La programamzione ad oggetti (OOP - Object Oriented Programming) è un
paradigma di programmazione che si basa sull'organizzare i dati in oggetti e
manipolarli unicamente tramite i metodi esposti da essi. Mentre nella
programmazione procedurale (tradizionale) occorre definire le proprietà di ogni
oggetto, nella programmazione OOP si crea una classe con le proprietà e si
associano gli oggetti alla classe. In questo modo si evita di scriverle per ogni
oggetto. *)

// Le classi

(* Per creare un oggetto dobbiamo definire una classe che lo rappresenterà, le
funzioni definite all'interno della classe sono chiamati metodi della classe. Ad
esempio, creiamo una classe che rappresentà un triangolo, i cui metodi ci
permettono di calcolarne area e perimetro. *)

type Triangle() =
    member self.Area(b, h) = b * h / 2.0
    member self.Perimeter(a, b, c) = a + b + c

(* Come puoi vedere ogni metodo della classe ha come primo parametro self,
questo ci permette di identificare attributi e metodi all'interno della classe
stessa, equivalente di this in uso in altri linguaggi. Concettualmente
un'oggetto è un'instanza di una classe. Instanziamo la classe Triangle per
creare l'oggetto e usiamo i suoi metodi. *)

let triangle = Triangle()

triangle.Area(3.0, 4.0) |> printfn "Area del triangolo: %A"
triangle.Perimeter(5.0, 3.0, 5.0) |> printfn "Perimetro del triangolo: %A"

(* Per calcolare area e perimetro dobbiamo passare di volta in volta le
informazioni sulle misure di base, altezze e lati del triangolo, che funziona ma
è concettualmente sbagliato, un'oggetto deve contenere le proprie informazioni
al suo interno, in apposite variabili chiamate attributi. Possiamo definire gli
attributi della classe all'interno di un metodo costruttore che in F# è
la definizione della classe *)

type Triangle'(a, b, c, h) =
    member self.Area() = b * h / 2.0
    member self.Perimeter() = a + b + c

let triangle' = Triangle'(5.0, 2.0, 5.0, 4.5)

triangle'.Area() |> printfn "Area del triangolo: %A"
triangle'.Perimeter() |> printfn "Perimetro del triangolo: %A"
printfn $"{triangle'}"

(* In F# per il pretty printing implementiamo il metodo .ToString()
Inoltre la documentazione viene scritta usando /// per un commento *)

/// La classe triangolo
type Triangle''(a, b, c, h) =

    /// Calcola l'area
    member self.Area() = b * h / 2.0

    /// Calcola il perimetro
    member self.Perimeter() = a + b + c

    /// Il ToString
    override self.ToString() =
        $"Area: {self.Area()}; Perimetro: {self.Perimeter()}"


let triangle'' = Triangle''(5.0, 2.0, 5.0, 4.5)
printfn $"{triangle''}"

// Ereditarietà e polimorfismo

(* Nella OOP è possibile derivare classi da altre classi. La classe di base
viene chiamata classe padre (parent class), mentre la classe derivata viene
chiamata classe figlia (child class). Definiamo una classe padre per
rappresentare una figura geometrica.

In F# è estremamente sconsigliato l'uso dell'ereditarietà, al massimo
si tendono ad usare delle interfacce 

Nota: l'uso delle tuple come nell'esempio di riferimento in python è 
estremamente prono ad errori e non idiomatico *)

type IShape =
    abstract member Area: unit -> double
    abstract member Perimeter: unit -> double

type Square(side) =
    // `interface <Interface> with` non è necessario, capisce automaticamente
    interface IShape with
        member self.Area() = side * side
        member self.Perimeter() = side * 4.0

// Il ' si usa in quante `base` è una keyword
type Rectangle(base', height) =
    interface IShape with
        member self.Area() = base' * height
        member self.Perimeter() = base' * height * 2.0

let printArea (shape: IShape) = printfn $"{shape.Area()}"

Rectangle(5.0, 3.0) |> printArea
Square(4.0) |> printArea
