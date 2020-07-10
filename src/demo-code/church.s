import List

# The types precisely match the ones of F# (even same letters).
# To check this, just copy to them to vscode and remove the trailing "in" keyword.

let cNumerals =
    ## Church numeral               ## Type (copied and pasted from the Type tab).
    let c0 f x = x in               # 'a -> 'b -> 'b
    let c1 f x = f x in             # ('a -> 'b) -> 'a -> 'b
    let c2 f x = f (f x) in         # ('a -> 'a) -> 'a -> 'a
    let c3 f x = f (f (f x)) in     # ('a -> 'a) -> 'a -> 'a
    let c4 f x = f (f (f (f x))) in # ('a -> 'a) -> 'a -> 'a

    let cSucc cn f x = f (cn f x) in            # (('a -> 'b) -> 'c -> 'a) -> ('a -> 'b) -> 'c -> 'b
    let cPlusS cN cM f x = (cM cSucc) cN f x in # 'a -> (((('b -> 'c) -> 'd -> 'b) -> ('b -> 'c) -> 'd -> 'c) -> 'a -> 'e -> 'f -> 'g) -> 'e -> 'f -> 'g
    let cPlus cN cM f x = (cM f) (cN f x) in    # ('a -> 'b -> 'c) -> ('a -> 'c -> 'd) -> 'a -> 'b -> 'd
    let cTimes cN cM f x = cM (cN f) x in       # ('a -> 'b) -> ('b -> 'c -> 'd) -> 'a -> 'c -> 'd

    let addOne x = x + 1 in

    # Print the result of a bunch of testcases.
    # ListReverse makes the print appear in the same order as
    # we write them down (not reversed).
    listMap print (listReverse [
        c4 addOne 0,
        cSucc c0 addOne 0,
        cPlus c2 c4 addOne 0,
        cTimes c2 c4 addOne 0,
        cTimes (cPlus c0 c2) (cSucc c4) addOne 0 
    ])
    ni ni ni ni ni ni ni ni ni ni
in

let cBooleans =
    let cFalse = \a b. b in # 'a -> 'b -> 'a
    let cTrue  = \a b. a in # 'a -> 'b -> 'b
    listMap print (listReverse [
        cTrue "true" "false",
        cFalse "true" "false"
    ])
    ni ni
in

cNumerals
# cBooleans

ni ni
