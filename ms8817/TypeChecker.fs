module TypeChecker

open TokeniserStub
open Parser

type Base =
    | Int
    | Bool
    | String
    | NullType // To terminate lists.

and Type =
    | Base of Base
    | Gen of int // Generic type with unique identifier.
    | Fun of Type * Type
    | Pair of Type * Type // Used for lists.

type Var = string * Type

/// mappings: map every identifier to its type.
/// uniqueId: give a new unique id when used as parameter to newId.
type Context = {
    mappings: Var list;
    uniqueId: int;
}

/// Return a tuple containing a unique id and the new context.
let newId ctx = ctx.uniqueId, {ctx with uniqueId = ctx.uniqueId + 1}

/// Extend a context with a new variable and its type. If the variable is
/// already present, it gets overridden (this allows variable shadowing in inner
/// scopes).
let extend ctx varName varType =
    let tryIndex =
        List.tryFindIndex (fun (name, _) -> name = varName) ctx.mappings
    match tryIndex with
    | None -> // New variable.
        {ctx with mappings = (varName, varType) :: ctx.mappings}
    | Some idx -> // Replace the previous one.
        let l, r = List.splitAt idx ctx.mappings
        {ctx with mappings = l @ [varName, varType] @ List.tail r}

type Subst = {
    wildcard: int;
    newType: Type;
}

/// Apply a substitution: set all the occurrences of a specific wildcard to the
/// type given, and return the new list of mappings.
let rec specialise mappings sub : Var list =
    let matchWildcard wildcard var =
        match var with
        | _, Gen g when g = wildcard -> true
        | _ -> false
    List.tryFindIndex (matchWildcard sub.wildcard) mappings
    |> function
        | None -> mappings
        | Some idx ->
            let l, r = List.splitAt idx mappings
            let varName, _ = mappings.[idx]
            l @ [varName, sub.newType] @ List.tail r

/// Return a new context with the applied substitutions.
let applySubstitutions ctx subs =
    (ctx, subs) ||> List.fold (
        fun ctx sub -> {ctx with mappings = specialise ctx.mappings sub}
    )

/// Try to unify two types, and if successful returns a list of substitutions
/// needed to do so.
/// If this is not possible, return error.
let rec unify t1 t2 : Result<Subst list, string> =
    match t1, t2 with
    | Base b1, Base b2 when b1 = b2 -> Ok []
    | Fun (l, r), Fun (l', r') | Pair (l, r), Pair (l', r') ->
        // Try to unify both sides.
        match unify l l' with
        | Error e -> Error e
        | Ok subs -> match unify r r' with
                     | Error e -> Error e
                     | Ok subs' -> Ok <| subs @ subs'
    | t, Gen g | Gen g, t ->
        // Can specialise the generic type g into the type t.
        Ok <| [{wildcard = g; newType = t}]
    | _ -> Error <| sprintf "Types %A and %A are not compatable" t1 t2

/// Apply a given substitution list to a type, and return the resulting type.
let rec apply subs t =
    match t with
    | Base _ -> t
    | Gen wildcard ->
        match List.tryFind (fun s -> s.wildcard = wildcard) subs with
        | None -> Gen wildcard
        | Some s -> s.newType
    | Fun (t1, t2) | Pair (t1, t2) ->
        Fun (apply subs t1, apply subs t2)

/// Try to lookup the type of an identifier.
let lookUpType ctx name =
    List.tryFind (fun (varName, _) -> name = varName) ctx.mappings

// Arithmetic binary operators split by type.
let int2int   = [Plus; Minus; Div; Mult]
let int2bool  = [Greater; GreaterEq; Less; LessEq; Equal]
let bool2bool = [And; Or]
let isBinaryOp op = List.contains op (int2int @ int2bool @ bool2bool)

//=====================//
// Inference functions //
//=====================//

let inferIdentifier ctx name =
    match lookUpType ctx name with
    | None -> Error <| sprintf "Identifier %s is not bound" name
    | Some (_, t) -> Ok ([], t)

let inferBinOp op =
    let isInt2Int   = List.tryFind ((=) op) int2int
    let isInt2Bool  = List.tryFind ((=) op) int2bool
    let isBool2Bool = List.tryFind ((=) op) bool2bool
    match isInt2Int, isInt2Bool, isBool2Bool with
    | Some _, None, None -> Ok ([], Fun(Base Int, Fun(Base Int, Base Int)))
    | None, Some _, None -> Ok ([], Fun(Base Int, Fun(Base Int, Base Bool)))
    | None, None, Some _ -> Ok ([], Fun(Base Bool, Fun(Base Bool, Base Bool)))
    | _ -> impossible "type checker, BuiltinFunc binary op"

let rec inferIfExp ctx c t e =
    let i1 = infer ctx c
    let i2 = infer ctx t
    let i3 = infer ctx e
    match i1, i2, i3 with
    | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
    | Ok (s1, t1), Ok (s2, t2), Ok (s3, t3) ->
        let rs4 = unify t1 (Base Bool) // Make sure the condition is bool.
        let rs5 = unify t2 t3 // Make sure both branches have same type.
        match rs4, rs5 with
        | Error e, _ | _, Error e -> Error e
        | Ok s4, Ok s5 -> Ok (s1 @ s2 @ s3 @ s4 @ s5, apply s5 t2)

and inferSeqExp ctx head tail =
    let i1 = infer ctx head
    let i2 = infer ctx tail
    match i1, i2 with
    | Error e, _ | _, Error e -> Error e
    | Ok (s1, t1), Ok (s2, t2) -> Ok (s1 @ s2, Pair (t1, t2))

and inferFuncApp ctx arg1 arg2 =
    let newWildcardId, ctx' = newId ctx
    let newWildcard = Gen newWildcardId // Return type of the func application.
    match infer ctx' arg1 with
    | Error e -> Error e
    | Ok (s1, t1) ->
        // Infer the type of the second argument applying the substitutions from
        // the first inference.
        match infer (applySubstitutions ctx' s1) arg2 with
        | Error e -> Error e
        | Ok (s2, t2) ->
            // We expect the t1 to be a lambda that takes t2 and returns
            // a new type. Hence we unify t1 with t2 -> newType.
            match unify (apply s2 t1) (Fun (t2, newWildcard)) with
            | Error e -> Error e
            | Ok s3 -> Ok (s3 @ s2 @ s1, apply s3 newWildcard)

and inferLambdaExp ctx lam =
    let newWildcardId, ctx' = newId ctx
    let newWildcard = Gen newWildcardId // For the lambda bound variable.
    let ctx'' = extend ctx' lam.LambdaParam newWildcard
    match infer ctx'' lam.LambdaBody with
    | Error e -> Error e
    | Ok (s1, t1) -> Ok (s1, Fun(apply s1 newWildcard, t1))

and inferFuncDefExp ctx def =
    // We infer the type of the body without keeping the function name in
    // our context. This makes recursion impossible.
    // TODO: remove this limitation?
    match infer ctx def.FuncBody with
    | Error e -> Error e
    | Ok (s1, t1) ->
        let ctx' = applySubstitutions ctx s1
        match infer (extend ctx' def.FuncName t1) def.Rest with
        | Error e -> Error e
        | Ok (s2, t2) -> Ok (s1 @ s2, t2)
    
/// Infer the type of an ast, and return the substitutions, together with the
/// type of the ast.
and infer ctx ast : Result<Subst list * Type, string> =
    match ast with
    | Null -> Ok ([], Base NullType)
    | Literal (IntLit _)    -> Ok ([], Base Int)
    | Literal (BoolLit _)   -> Ok ([], Base Bool)
    | Literal (StringLit _) -> Ok ([], Base String)
    | Identifier name -> inferIdentifier ctx name
    | BuiltInFunc op when isBinaryOp op -> inferBinOp op
    | BuiltInFunc StrEq -> Ok ([], Fun(Base String, Fun(Base String, Base Bool)))
    | IfExp (c, t, e) -> inferIfExp ctx c t e
    | SeqExp (head, tail) -> inferSeqExp ctx head tail
    | FuncApp (arg1, arg2) -> inferFuncApp ctx arg1 arg2
    | LambdaExp lam -> inferLambdaExp ctx lam
    | FuncDefExp def -> inferFuncDefExp ctx def

let typeCheck ast =
    let ctx = {mappings = []; uniqueId = 0}
    infer ctx ast |> Result.map (fun (_, t) -> t) // Just return the type.

// TODO:
// - add lists support
// - add other builtin funcs
// - tests, many of them
// - cleanup
// - support recursion
