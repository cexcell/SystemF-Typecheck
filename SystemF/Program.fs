// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Typechecker
open System

[<EntryPoint>]
let main argv = 
    let type1 = ArrowType(TVar("x"), TVar("y"));
    let type2 = ArrowType(TVar("x"), TVar("y"));
    let result = isEqual(type1, type2)
    printfn "%A" result
    Console.ReadKey() |> ignore
    0