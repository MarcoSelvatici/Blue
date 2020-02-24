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

/// Try to unify two types, and if successful returns a list of substitutions
/// needed to do so.
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

/// Apply a given substitution list to a type, and return the resulting type.
let rec apply subs t =
    match t with
    | Base _ -> t
    | Gen wildcard ->
        match List.tryFind (fun s -> s.wildcard = wildcard) subs with
        | None -> Gen wildcard
        | Some s -> s.newType
    | Fun (t1, t2) ->
        Fun (apply subs t1, apply subs t2)

/// Try to lookup the type of an identifier.
let lookUpType ctx name =
    List.tryFind (fun (varName, _) -> name = varName) ctx.mappings

/// Infer the type of an ast, and return the substitutions, together with the
/// type of the ast.
let rec infer ctx ast : Result<Subst list * Type, string> =
    let int2int   = [Plus; Minus; Div; Mult]
    let int2bool  = [Greater; GreaterEq; Less; LessEq; Equal]
    let bool2bool = [And; Or]
    match ast with
    | Literal (IntLit _)    -> Ok ([], Base Int)
    | Literal (BoolLit _)   -> Ok ([], Base Bool)
    | Literal (StringLit _) -> Ok ([], Base String)
    | Identifier name ->
        match lookUpType ctx name with
        | None -> Error <| sprintf "Identifier %s is not bound" name
        | Some (_, t) -> Ok ([], t)
    | IfExp (c, t, e) ->
        let i1 = infer ctx c
        let i2 = infer ctx t
        let i3 = infer ctx e
        match i1, i2, i3 with
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
        | Ok (s1, t1), Ok (s2, t2), Ok (s3, t3) ->
            let rs4 = unify ctx t1 (Base Bool) // Make sure the condition is bool.
            let rs5 = unify ctx t2 t3 // Make sure both branches have same type.
            match rs4, rs5 with
            | Error e, _ | _, Error e -> Error e
            | Ok s4, Ok s5 -> Ok (s1 @ s2 @ s3 @ s4 @ s5, apply s5 t2)

    // TODO: this explicit handling of binary operators should not be necessary.
    // but it forces the operator to have explicit arguments (already done by
    // the parser though).
    | FuncApp (FuncApp (BuiltInFunc binOp, arg1), arg2) -> // Binary operators.
        let i1 = infer ctx arg1
        let i2 = infer ctx arg2
        match i1, i2 with
        | Error e, _ | _, Error e -> Error e
        | Ok (s1, t1), Ok (s2, t2) ->
            let isInt2Int   = List.tryFind ((=) binOp) int2int
            let isInt2Bool  = List.tryFind ((=) binOp) int2bool
            let isBool2Bool = List.tryFind ((=) binOp) bool2bool
            match isInt2Int, isInt2Bool, isBool2Bool with
            | Some _, None, None -> // Int to Int.
                let rs3 = unify ctx t1 (Base Int)
                let rs4 = unify ctx t2 (Base Int)
                match rs3, rs4 with
                | Error e, _ | _, Error e -> Error e
                | Ok s3, Ok s4 -> Ok (s1 @ s2 @ s3 @ s4, Base Int)
            | None, Some _, None -> // Int to Bool.
                let rs3 = unify ctx t1 (Base Int)
                let rs4 = unify ctx t2 (Base Int)
                match rs3, rs4 with
                | Error e, _ | _, Error e -> Error e
                | Ok s3, Ok s4 -> Ok (s1 @ s2 @ s3 @ s4, Base Bool)
            | None, None, Some _ -> // Bool to Bool.
                let rs3 = unify ctx t1 (Base Bool)
                let rs4 = unify ctx t2 (Base Bool)
                match rs3, rs4 with
                | Error e, _ | _, Error e -> Error e
                | Ok s3, Ok s4 -> Ok (s1 @ s2 @ s3 @ s4, Base Bool)
            | _ -> Error <| sprintf "Binary opertator %A is not supported" binOp


let typeCheck ast =
    let ctx = {mappings = []; uniqueId = 0}
    infer ctx ast
