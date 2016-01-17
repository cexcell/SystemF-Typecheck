module Typechecker

    exception TypeError of string

    //let lambdaSym = System.Char.ConvertFromUtf32(955)
    //let capLambdaSym = System.Char.ConvertFromUtf32(923)
    //let forallSym = System.Char.ConvertFromUtf32(8704)
    let lambdaSym = "\\"
    let capLambdaSym = "/\\"
    let forallSym = "\\/"

    type TypeVariable = string

    type Type = 
        ArrowType of Type * Type 
        | ForAll of TypeVariable * Type 
        | TVar of TypeVariable

        override this.ToString() = 
            match this with
            | ArrowType(t1, t2) ->
                "(" + t1.ToString() + " -> " + t2.ToString() + ")"
            | ForAll(tVar, t) -> 
                let str = t.ToString();
                "(" + forallSym + tVar + ". " + str + ")"
            | TVar(name) -> name

    let (|->) t1 t2 = ArrowType(t1, t2)
    
    let rec private equalTypes (context: (TypeVariable * TypeVariable) list, t1: Type, t2: Type) = 
        match t1 with
           | ArrowType(l1, r1) -> 
            match t2 with
                | ArrowType(l2, r2) -> equalTypes(context, l1, l2) && equalTypes(context, r1, r2)
                | _ -> false
           | ForAll(tvar1, t1) ->
            match t2 with
                | ForAll(tvar2, t2) -> equalTypes((tvar1, tvar2)::context, t1, t2)
                | _ -> false
           | TVar(tvar1) ->
            match t2 with
                | TVar(tvar2) -> 
                    try 
                        let (find1, find2) = List.find (fun (l, r) -> l = tvar1) context
                        find2 = tvar2
                    with
                        | ex -> tvar1 = tvar2
                | _ -> false

    let (=?): Type -> Type -> bool = 
        fun t1 t2 -> equalTypes([], t1, t2)

    type ExpressionVariable = string

    type Expression = 
        EVar of ExpressionVariable 
        | Abstraction of ExpressionVariable * Type * Expression 
        | TAbstraction of TypeVariable * Expression 
        | ApplTermTerm of Expression * Expression 
        | ApplTypeTerm of Expression * Type

        override this.ToString() = 
            match this with
            | EVar(var) -> var
            | Abstraction(eVar, t, e) -> 
                let typeStr = t.ToString();
                let termStr = e.ToString();
                "(" + lambdaSym + eVar + ": (" + typeStr + "). " + termStr + ")"
            | TAbstraction(tVar, e) ->
                "(" + capLambdaSym + tVar + ". " + e.ToString() + ")"
            | ApplTermTerm(e1, e2) -> 
                "(" + e1.ToString() + " " + e2.ToString() + ")"
            | ApplTypeTerm(e, t) ->
                "(" + e.ToString() + " " + t.ToString() + ")"

    let (@>>) term1 term2 = ApplTermTerm(term1, term2)
    let (@<>) term t = ApplTypeTerm(term, t)

    type Assumption = ExpressionAbstraction of ExpressionVariable * Type | TypeAbstraction of TypeVariable

    type Context = Assumption list

    let rec getFromContext: ExpressionVariable -> Context -> Type option =
        fun variable context -> 
            match context with
            | head::tail -> match head with
                            | ExpressionAbstraction(eVar, t) -> if (eVar = variable) then Some t else getFromContext variable tail
                            | TypeAbstraction(_) -> getFromContext variable tail
            | [] -> None

    let addExpressionVariable: ExpressionVariable -> Type -> Context -> Context =
        fun eVar t ctx -> ExpressionAbstraction(eVar, t)::ctx

    let addTypeVariable: TypeVariable -> Context -> Context =
        fun typeVar ctx -> TypeAbstraction(typeVar)::ctx

    let initContext: Context = []

    let rec substitution: TypeVariable -> Type -> Type -> Type = 
        fun varName what target ->
            match target with
                | ArrowType(t1, t2) -> (substitution varName what t1) |-> (substitution varName what t2)
                | ForAll(tVar, t) -> if (tVar <> varName) then ForAll(tVar, substitution varName what t) else ForAll(tVar, t)
                | TVar(name) -> if (name = varName) then what else TVar(name)

    let rec private helper: Expression -> Context -> Type = 
            fun expr ctx -> 
                match expr with
                    | EVar(name) -> 
                        let maybeT = getFromContext name ctx
                        match maybeT with
                            | Some(t) -> t
                            | None -> raise(TypeError("No such variable in context: " + name))
                    | Abstraction(eVar, t, e) ->
                        let newContext = addExpressionVariable eVar t ctx
                        let newType = helper e newContext
                        t |-> newType
                    | TAbstraction(tVar, e) ->
                        let newContext = addTypeVariable tVar ctx
                        let newType = helper e newContext
                        ForAll(tVar, newType)
                    | ApplTermTerm(e1, e2) -> 
                        let t1 = helper e1 ctx
                        let t2 = helper e2 ctx
                        match t1 with
                            | ArrowType(left, right) -> if (left =? t2)
                                                        then right 
                                                        else raise(TypeError("Term <" + e1.ToString() + "> cannot be applied to term <" + e2.ToString() + ">"))
                            | _ -> raise(TypeError("Term <" + e1.ToString() + "> cannot be applied to term <" + e2.ToString() + ">"))
                    | ApplTypeTerm(e, t) ->
                        let expressionType = helper e ctx
                        match expressionType with
                            | ForAll(typeVar, t1) -> substitution typeVar t t1
                            | _ -> raise(TypeError("Type <" + t.ToString() + "> cannot be applied to tern <" + e.ToString() + ">"))

    let typeInferenceCtx: Context -> Expression -> Type = 
        fun context expression -> helper expression context
    
    let typeInference: Expression -> Type = 
        fun expression ->
            let context = initContext
            helper expression context