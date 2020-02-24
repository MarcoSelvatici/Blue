module TypeChecker

open TokeniserStub
open Parser

type Base =
    | Int
    | Bool
    | String
    // | List of ... TODO

/// Typed variable. A string
type Type =
    | Base of Base
    | Gen of int // Generic type with unique identifier.
    | Fun of Type * Type

type Var = string * Type

/// mappings: map every identifier to its type.
/// uniqueId: give a new unique id when used as parameter to newId.
type Context = {
    mappings: Var list;
    uniqueId: int;
}

/// Return a tuple containing a unique id and the new context.
let newId ctx = ctx.uniqueId, {ctx with uniqueId = ctx.uniqueId + 1}

/// Extend a context with a new variable and its type.
let extend ctx id idType = {ctx with mappings = (id, idType) :: ctx.mappings}

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

/// Try to unify two types, and if successful returns a list of substitutions.
/// If this is not possible, return error.
let rec unify ctx t1 t2 : Result<Subst list, string> =
    match t1, t2 with
    | Base b1, Base b2 when b1 = b2 -> Ok []
    | Gen g1, Gen g2 when g1 = g2 -> Ok []
    | Fun (l, r), Fun (l', r') ->
        // Try to unify both sides.
        match unify ctx l l' with
        | Error e -> Error e
        | Ok subs -> match unify (applySubstitutions ctx subs) r r' with
                     | Error e -> Error e
                     | Ok subs' -> Ok <| subs @ subs'
    | Base b, Gen g
    | Gen g, Base b ->
        // Can specialise the generic type g into the base type b.
        Ok <| [{wildcard = g; newType = Base b}]
    | _ -> Error <| sprintf "Types %A and %A are not compatable" t1 t2

/// Infer the type of an ast, and return the substitutions, together with the
/// type of the ast.
let rec infer ctx ast : Result<Subst list * Type, string> =
    match ast with
    | Literal (IntLit _)    -> Ok ([], Base Int)
    | Literal (BoolLit _)   -> Ok ([], Base Bool)
    | Literal (StringLit _) -> Ok ([], Base String)


let typeCheck ast =
    let ctx = {mappings = []; uniqueId = 0}
    match infer ctx ast with
    | Error e -> printfn "%s" e
    | Ok t -> printfn "%A" t
