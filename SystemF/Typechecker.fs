module Typechecker

    type TypeVariable = string

    type Type = ArrowType of Type * Type | ForAll of TypeVariable * Type | TVar of TypeVariable
    
    let rec private equalTypes (context: (TypeVariable * TypeVariable) list, t1: Type, t2: Type) = 
        match t1 with
           | ArrowType(l1, r1) -> 
            match t2 with
                | ArrowType(l2, r2) -> equalTypes(context, l1, l2) && equalTypes(context, r1, r2)
                | ForAll(_, _) -> false
                | TVar(_) -> false
           | ForAll(tvar1, t1) ->
            match t2 with
                | ArrowType(_, _) -> false
                | ForAll(tvar2, t2) -> equalTypes((tvar1, tvar2)::context, t1, t2)
                | TVar(_) -> false
           | TVar(tvar1) ->
            match t2 with
                | ArrowType(_, _) -> false
                | ForAll(_, _) -> false
                | TVar(tvar2) -> 
                    try 
                        let (find1, find2) = List.find (fun (l, r) -> l = tvar1) context
                        find2 = tvar2
                    with
                        | ex -> tvar1 = tvar2

    let isEqual(t1: Type, t2: Type) = equalTypes([], t1, t2)