// Learn more about F# at http://fsharp.org

open System

let add a b = a + b
let multiply a b = a * b

let product = List.fold multiply 1.0
let sum = List.fold add 0.0

let rec askForFloat prompt = 
    printf prompt
    let textInput = Console.ReadLine()
    if textInput = "n" then None else
        match Double.TryParse(textInput) with
        | true, value -> Some value
        | false, _ -> printfn "I understand things like '1.0', '2.4' and '2', however I don't understand '%s'" textInput
                      askForFloat prompt

let rec askUserForNumbers numbersSoFar = 
    match askForFloat "Please enter a number (or 'n' if finished): " with
    | Some value -> askUserForNumbers (value :: numbersSoFar)
    | None -> numbersSoFar

[<EntryPoint>]
let main argv =
    let numbers = askUserForNumbers []
    printfn "Sum of %A is %f" numbers (sum numbers)
    0 // return an integer exit code