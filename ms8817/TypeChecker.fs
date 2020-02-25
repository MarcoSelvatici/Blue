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
type Context = Var list

// Note: using a mutable value is not the only way to generate unique ids.
// One could return the next uinque id at the end of each infer call, but this
// would make the whole code more cluttery, since there would be the need to
// handle (or just "forward") an extra param in many functions that would not
// use it otherwise. Both solutions have pros and cons. 
let mutable uniqueId = 0;
/// Return new unique id.
let newId () =
    uniqueId <- uniqueId + 1
    uniqueId
let resetUniqueId () = uniqueId <- 0

/// Extend a context with a new variable and its type. If the variable is
/// already present, it gets overridden (this allows variable shadowing in inner
/// scopes).
let extend ctx varName varType =
    let tryIndex =
        List.tryFindIndex (fun (name, _) -> name = varName) ctx
    match tryIndex with
    | None -> // New variable.
        (varName, varType) :: ctx
    | Some idx -> // Replace the previous one.
        let l, r = List.splitAt idx ctx
        l @ [varName, varType] @ List.tail r

type Subst = {
    wildcard: int;
    newType: Type;
}

/// Return a new context with the applied substitutions.
let applySubstitutions ctx subs =
    /// Apply a substitution: set all the occurrences of a specific wildcard to the
    /// type given, and return the new context.
    let rec specialise ctx sub : Var list =
        let matchWildcard wildcard var =
            match var with
            | _, Gen g when g = wildcard -> true
            | _ -> false
        List.tryFindIndex (matchWildcard sub.wildcard) ctx
        |> function
            | None -> ctx
            | Some idx ->
                let l, r = List.splitAt idx ctx
                let varName, _ = ctx.[idx]
                l @ [varName, sub.newType] @ List.tail r
    (ctx, subs) ||> List.fold specialise

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
                     | Ok subs' -> Ok <| subs' @ subs
    | t, Gen g | Gen g, t ->
        // Can specialise the generic type g into the type t.
        Ok <| [{wildcard = g; newType = t}]
    | _ -> Error <| sprintf "Types %A and %A are not compatable" t1 t2

/// Apply a given substitution list to a type, and return the resulting type.
let rec apply subs t =
    /// Recursively substitute a wildcard with its type if any.
    /// If there is no substitution possible, return the wildcard as is.
    let rec subWildcard wildcard =
        match List.tryFind (fun s -> s.wildcard = wildcard) subs with
        | None -> Gen wildcard
        | Some s -> match s.newType with
                    | Gen wildcard' -> subWildcard wildcard'
                    | _ -> s.newType
    match t with
    | Base _ -> t
    | Gen wildcard -> subWildcard wildcard
    | Fun (t1, t2) | Pair (t1, t2) ->
        Fun (apply subs t1, apply subs t2)

/// Try to lookup the type of an identifier.
let lookUpType ctx name =
    List.tryFind (fun (varName, _) -> name = varName) ctx

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
    let newWildcardId = newId ()
    let newWildcard = Gen newWildcardId // Return type of the func application.
    match infer ctx arg1 with
    | Error e -> Error e
    | Ok (s1, t1) ->
        // Infer the type of the second argument applying the substitutions from
        // the first inference.
        match infer (applySubstitutions ctx s1) arg2 with
        | Error e -> Error e
        | Ok (s2, t2) ->
            // We expect the t1 to be a lambda that takes t2 and returns
            // a new type. Hence we unify t1 with t2 -> newType.
            match unify (apply s2 t1) (Fun (t2, newWildcard)) with
            | Error e -> Error e
            | Ok s3 -> Ok (s1 @ s2 @ s3, apply s3 newWildcard)

and inferLambdaExp ctx lam =
    let newWildcardId = newId ()
    let newWildcard = Gen newWildcardId // For the lambda bound variable.
    let ctx' = extend ctx lam.LambdaParam newWildcard
    match infer ctx' lam.LambdaBody with
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
    //| BuiltInFunc Head -> Ok ([], Fun (Gen))
    | IfExp (c, t, e) -> inferIfExp ctx c t e
    | SeqExp (head, tail) -> inferSeqExp ctx head tail
    | FuncApp (arg1, arg2) -> inferFuncApp ctx arg1 arg2
    | LambdaExp lam -> inferLambdaExp ctx lam
    | FuncDefExp def -> inferFuncDefExp ctx def

let typeCheck ast =
    resetUniqueId ()
    infer [] ast |> Result.map (fun (_, t) -> t) // Just return the type.

// TODO:
// - add other builtin funcs
// - tests, many of them
// - cleanup
// - support recursion
