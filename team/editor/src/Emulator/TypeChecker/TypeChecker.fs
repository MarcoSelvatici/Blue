// Author: ms8817 (Marco Selvatici)

module TypeChecker

open SharedTypes
open Parser

//=======//
// Types //
//=======//

type Base =
    | Int
    | Bool
    | String

type Type =
    | Base of Base
    | Gen of int // Generic type with unique identifier.
    | Fun of Type * Type
    | Pair of Type * Type // Used for lists.

// Note: empty sequences have a generic type Pair(Gen _, Gen _).
// This is similar to the f# type of an empty list: val []: 'a list.

type Var = string * Type

/// Map every identifier to its type.
type Context = Var list

type Subst = {
    wildcard: int;
    newType: Type;
}

//==================//
// Helper functions //
//==================//

/// Pretty print the type.
let type2String t =
    let addIfNew (genId : int) genCtx =
        let genMap, newChar = genCtx
        match Map.tryFind genId genMap with
        | Some chr -> string chr, genCtx
        | None -> // Create new.
            let genMap = Map.add genId newChar genMap
            let newChar' = char ((int newChar) + 1)
            string newChar, (genMap, newChar')

    let rec print t genCtx =
        let rec printPair t (genCtx : Map<int, char> * char) =
            match t with
            | Pair (Gen _, Gen _) -> "", genCtx
            | Pair (head, Pair (Gen _, Gen _)) -> print head genCtx
            | Pair (head, tail) ->
                let lhs, genCtx = print head genCtx
                let rhs, genCtx = printPair tail genCtx
                (lhs + ", " + rhs), genCtx
            | _ ->  impossible "type2String"
        match t with
        | Base baseT -> (sprintf "%A" baseT), genCtx
        | Gen g ->
            let chr, genCtx = addIfNew g genCtx
            (sprintf "'%s" chr), genCtx
        | Fun (t1, t2) ->
            let lhs, genCtx =
                match t1 with
                | Base _ | Gen _ | Pair _ -> print t1 genCtx
                | _ ->
                     let str, genCtx = print t1 genCtx
                     sprintf "(%s)" str, genCtx
            let rhs, genCtx = print t2 genCtx
            lhs + " -> " + rhs, genCtx
        | Pair _ ->
            let str, genCtx = printPair t genCtx
            sprintf "[" + str + "]", genCtx
    let str, _ = print t (Map.empty, 'a')
    str

/// Return a new Generic identifier, and the updated uid.
let newGen uid = Gen uid, uid + 1

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

/// Return a new context with the applied substitutions.
let applyToCtx subs ctx =
    /// Apply a substitution: set all the occurrences of a specific wildcard to the
    /// type given, and return the new context.
    let rec specialise ctx sub : Var list =
        let matchWildcard wildcard var =
            match var with
            | _, Gen g when g = wildcard -> true
            | _ -> false
        match List.tryFindIndex (matchWildcard sub.wildcard) ctx with
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
    | _ -> Error <| sprintf "Types:\n%s\nand\n%s\nare not compatable" (type2String t1) (type2String t2)

/// Apply a given substitution list to a type, and return the resulting type.
let rec apply subs t =
    match t with
    | Base _ -> t
    | Gen wildcard ->
        match List.tryFind (fun s -> s.wildcard = wildcard) subs with
        | None -> Gen wildcard // No substitution found. Return wildcard.
        | Some s -> apply subs (s.newType) // Try to substitute the new type.
    | Fun (t1, t2) -> Fun (apply subs t1, apply subs t2)
    | Pair (t1, t2) -> Pair (apply subs t1, apply subs t2)

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

let inferNullType uid =
    // Assign generic type, since this will be used in empty sequences.
    let g, uid = newGen uid
    uid, Ok ([], g)

let inferIdentifier uid ctx name =
    match lookUpType ctx name with
    | None -> uid, Error <| sprintf "Identifier %s is not bound" name
    | Some (_, t) -> uid, Ok ([], t)

let inferBinOp op =
    let isInt2Int   = List.tryFind ((=) op) int2int
    let isInt2Bool  = List.tryFind ((=) op) int2bool
    let isBool2Bool = List.tryFind ((=) op) bool2bool
    match isInt2Int, isInt2Bool, isBool2Bool with
    | Some _, None, None -> Ok ([], Fun(Base Int, Fun(Base Int, Base Int)))
    | None, Some _, None -> Ok ([], Fun(Base Int, Fun(Base Int, Base Bool)))
    | None, None, Some _ -> Ok ([], Fun(Base Bool, Fun(Base Bool, Base Bool)))
    | _ -> impossible "type checker, inferBinOp"

let inferListOp uid op =
    let tHead, uid = newGen uid
    let tTail, uid = newGen uid
    match op with
    | Head -> uid, Ok ([], Fun (Pair (tHead, tTail), tHead))
    | Tail -> uid, Ok ([], Fun (Pair (tHead, tTail), tTail))
    | Size -> uid, Ok ([], Fun (Pair (tHead, tTail), Base Int))
    | Append ->
        let tNewHead, uid = newGen uid
        uid, Ok ([], Fun (tNewHead, Fun (Pair (tHead, tTail), Pair (tNewHead, Pair (tHead, tTail)))))
    | _ -> impossible "type checker, inferListOp"

let inferImplodeExplode uid f =
    // Return a generic list. This is a best effort type checking.
    let tHead, uid = newGen uid
    let tTail, uid = newGen uid    
    match f with
    | Explode -> uid, Ok ([], Fun (Base String, Pair (tHead, tTail)))
    | Implode -> uid, Ok ([], Fun (Pair (tHead, tTail), Base String))
    | _ -> impossible "type checker, inferImplodeExplode"

let inferBuiltInFunc uid f =
    let isListOp op = List.contains op [Head; Tail; Size; Append]
    match f with
    | op when isBinaryOp op -> uid, inferBinOp op
    | op when isListOp op -> inferListOp uid op
    | Implode | Explode -> inferImplodeExplode uid f
    | StrEq -> uid, Ok ([], Fun(Base String, Fun(Base String, Base Bool)))
    | Test ->
        let t, uid = newGen uid
        uid, Ok ([], Fun(t, Base Bool))
    | _ -> uid, Error <| sprintf "Type checking for %A is not implemented" f

let rec inferIfExp uid ctx c t e =
    let uid, i1 = infer uid ctx c
    let uid, i2 = infer uid ctx t
    let uid, i3 = infer uid ctx e
    match i1, i2, i3 with
    | Error e, _, _ | _, Error e, _ | _, _, Error e -> uid, Error e
    | Ok (s1, t1), Ok (s2, t2), Ok (s3, t3) ->
        let rs4 = unify t1 (Base Bool) // Make sure the condition is bool.
        let rs5 = unify t2 t3 // Make sure both branches have same type.
        match rs4, rs5 with
        | Error e, _ | _, Error e -> uid, Error e
        | Ok s4, Ok s5 -> uid, Ok (s1 @ s2 @ s3 @ s4 @ s5, apply s5 t2)

and inferSeqExp uid ctx head tail =
    let uid, i1 = infer uid ctx head
    let uid, i2 = infer uid ctx tail
    match i1, i2 with
    | Error e, _ | _, Error e -> uid, Error e
    | Ok (s1, t1), Ok (s2, t2) -> uid, Ok (s1 @ s2, Pair (t1, t2))

and inferFuncApp uid ctx f arg =
    let newWildcard, uid = newGen uid // Return type of the func application.
    match infer uid ctx f with
    | _, Error e -> uid, Error e
    | uid, Ok (s1, t1) ->
        // Infer the type of the second argument applying the substitutions from
        // the first inference.
        match infer uid (applyToCtx s1 ctx) arg with
        | uid, Error e -> uid, Error e
        | uid, Ok (s2, t2) ->
            // We expect the t1 to be a lambda that takes t2 and returns
            // a new type. Hence we unify t1 with t2 -> newType.
            match unify (apply s2 t1) (Fun (t2, newWildcard)) with
            | Error e -> uid, Error e
            | Ok s3 -> uid, Ok (s1 @ s2 @ s3, apply s3 newWildcard)

and inferLambdaExp uid ctx lam =
    let newWildcard, uid = newGen uid // For the lambda bound variable.
    let ctx' = extend ctx lam.LambdaParam newWildcard
    match infer uid ctx' lam.LambdaBody with
    | uid2, Error e -> uid2, Error e
    | uid2, Ok (s1, t1) -> uid2, Ok (s1, Fun(apply s1 newWildcard, t1))

and inferFuncDefExp uid ctx def =
    // Extend the context with a generic type for our function, this allows
    // recursion.
    let funcType, uid = newGen uid
    let ctx = (extend ctx def.FuncName funcType)
    match infer uid ctx def.FuncBody with
    | uid, Error e -> uid, Error e
    | uid, Ok (s1, t1) ->
        match unify t1 funcType with
        | Error e -> uid, Error e
        | Ok sFuncType ->
            let ctx = applyToCtx (sFuncType @ s1) ctx
            match infer uid ctx def.Rest with
            | uid, Error e -> uid, Error e
            | uid, Ok (s2, t2) -> uid, Ok (sFuncType @ s1 @ s2, t2)

/// Infer the type of an ast, and return the substitutions, together with the
/// type of the ast.
and infer uid ctx ast : int * Result<Subst list * Type, string> =
    match ast with
    | Literal (IntLit _)    -> uid, Ok ([], Base Int)
    | Literal (BoolLit _)   -> uid, Ok ([], Base Bool)
    | Literal (StringLit _) -> uid, Ok ([], Base String)
    | Null                  -> inferNullType uid
    | Identifier name       -> inferIdentifier uid ctx name
    | BuiltInFunc f         -> inferBuiltInFunc uid f
    | IfExp (c, t, e)       -> inferIfExp uid ctx c t e
    | SeqExp (head, tail)   -> inferSeqExp uid ctx head tail
    | FuncApp (f, arg)      -> inferFuncApp uid ctx f arg
    | LambdaExp lam         -> inferLambdaExp uid ctx lam
    | FuncDefExp def        -> inferFuncDefExp uid ctx def
    | _ -> uid, Error <| sprintf "Type checking for %A is not implemented" ast

let typeCheck (ast : Ast) : Result<Type, ErrorT> =
    infer 0 [] ast
    |> function // Just return the type.
       | _, Ok (_, t) -> Ok t
       | _, Error e -> Error <| TypeCheckerError e
