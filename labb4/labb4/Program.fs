(*
open System

type Tree =
    | Empty
    | Node of string * Tree * Tree

let rec f t ch =
    match t with
    | Empty -> Empty
    | Node(v,l,r) ->
        Node((string ch)+v, f l ch, f r ch)

let rec show t =
    match t with
    | Empty -> ()
    | Node(v,l,r) ->
        printf "%s " v
        show l
        show r

[<EntryPoint>]
let main args =
    let t =
        Node("x",
            Node("y",Empty,Empty),
            Node("z",Empty,Empty))

    printf "символ: "
    let c = Console.ReadLine().[0]

    let t2 = f t c
    show t2
    0
    *)

open System

type Tree =
    | Empty
    | Node of int * Tree * Tree

let rec sum tree =
    match tree with
    | Empty -> (0, 0)
    | Node(v, l, r) ->
        let (evenL, oddL) = sum l
        let (evenR, oddR) = sum r

        if v % 2 = 0 then
            (evenL + evenR + v, oddL + oddR)
        else
            (evenL + evenR, oddL + oddR + v)

[<EntryPoint>]
let main args =
    let tree =
        Node(5,
            Node(2, Empty, Empty),
            Node(8, Empty, Empty))

    let (evenSum, oddSum) = sum tree

    printf "четные = %d, нечетные = %d\n" evenSum oddSum

    if evenSum > oddSum then
        printf "больше сумма четных\n"
    elif oddSum > evenSum then
        printf "больше сумма нечетных\n"
    else
        printf "суммы равны\n"

    0