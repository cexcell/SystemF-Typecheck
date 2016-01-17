// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Typechecker
open System

[<EntryPoint>]
let main argv = 
    // basic test
    let type1 = TVar("int") |-> TVar("x") //int->x
    let type2 = TVar("int") |-> TVar("y") //int->y
    let type3 = TVar("int") |-> TVar("y") //int->y
    let type4 = ForAll("x", type1) //Vx. int -> x
    let type5 = ForAll("y", type3) //Vy. int -> y

    assert((type1 =? type2) = false)
    assert((type2 =? type3) = true)
    assert((type4 =? type5) = true)

    //nat

    let Nat = TVar "Nat"
    let zero = EVar "zero"
    let succ = EVar "succ"

    let contextNat: Context = [TypeAbstraction "Nat"; ExpressionAbstraction("zero", Nat); ExpressionAbstraction("succ", (Nat |-> Nat))]
    let natTypeInference = typeInferenceCtx contextNat

    let id = Abstraction("x", Nat, EVar "x")
    let polymorphId = TAbstraction("a", Abstraction("x", TVar "a", EVar "x"))
    let veryComplexNatFunction = Abstraction("x", Nat |-> Nat, Abstraction("y", Nat, EVar("x") @>> EVar("y")))

    //bool

    let Bool = ForAll("a", TVar "a" |-> (TVar "a" |-> TVar "a"))

    let True = TAbstraction("a", Abstraction("x", TVar("a"), Abstraction("y", TVar("a"), EVar "x")))
    let False = TAbstraction("a", Abstraction("x", TVar("a"), Abstraction("y", TVar("a"), EVar "y")))

    try
        //nat tests
        let succTest = Nat =? (natTypeInference (succ @>> (succ @>> zero)))
        let idTest = (Nat |-> Nat) =? (natTypeInference id)
        let polymorphIdTest = ((ForAll("A", TVar "A" |-> TVar "A")) =? (natTypeInference polymorphId))
        let complexNatTest = ((Nat |-> Nat) |-> (Nat |-> Nat) =? (natTypeInference veryComplexNatFunction))

        assert(succTest)
        assert(idTest)
        assert(polymorphIdTest)
        assert(complexNatTest)

        // bool tests
        let expr = (((EVar "x") @<> (TVar "X")) @>> (EVar "F")) @>> (EVar "T")
        let not = Abstraction("x", Bool, TAbstraction("X", Abstraction("T", TVar "X", Abstraction("F", TVar "X", expr))))
        
        assert(Bool =? (typeInference False))
        assert(Bool =? (typeInference True))
        assert((Bool |-> Bool) =? (typeInference not))

        printfn "Success"
    with
        | e -> printfn "%A" e


    Console.ReadKey() |> ignore
    0