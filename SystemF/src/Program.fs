// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Typechecker
open System

[<EntryPoint>]
let main argv = 
    let type1 = TVar("int") |-> TVar("x") //int->x
    let type2 = TVar("int") |-> TVar("y") //int->y
    let type3 = TVar("int") |-> TVar("y") //int->y
    let type4 = ForAll("x", type1) //Vx. int -> x
    let type5 = ForAll("y", type3) //Vy. int -> y

    assert((type1 =? type2) = false)
    assert((type2 =? type3) = true)
    assert((type4 =? type5) = true)
    
    printfn "Success"
    Console.ReadKey() |> ignore
    0