// Learn more about F# at http://fsharp.org

open System

type Token =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Number of float

let isNumeric (number: string) = Double.TryParse(number) |> fst

let reverseList items =
     let rec reverse forward reversed =
         match forward with
         | x :: xs -> reverse xs (x :: reversed)
         | [] -> reversed
     reverse items []

let parseCalculation (calculation: string) =
    let rec parse (rest: string []) (tokens: Token list) =
        let parseRest token = parse (rest |> Array.tail) (token :: tokens)

        match rest with
        | [||] -> tokens
        | items ->
            match items |> Array.head with
            | "+" -> parseRest Add 
            | "-" -> parseRest Subtract
            | "*" -> parseRest Multiply
            | "/" -> parseRest Divide
            | number when number |> isNumeric ->
                parseRest (Number (float number))
            | _ -> tokens
    
    let tokens =
        calculation.Split [|' '|]
        |> Array.filter (fun item -> item.Length <> 0)

    (parse tokens []) |> reverseList

[<EntryPoint>]
let main argv =
    printf "Calc> "
    let input = Console.ReadLine()
    let stack = parseCalculation input
    printfn "Tokens are: %A" (stack)
    0 // return an integer exit code