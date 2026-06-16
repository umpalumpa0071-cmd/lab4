 (* open System

type Tree =
    | Empty
    | Node of string * Tree * Tree

let rec generateTree depth =
    if depth <= 0 then Empty
    else
        let rand = Random()
        let value = 
            let chars = "abcdefghijklmnopqrstuvwxyz"
            string chars.[rand.Next(chars.Length)]
        Node(value, generateTree (depth - 1), generateTree (depth - 1))

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
    printf "глубина дерева: "
    let depth = Console.ReadLine() |> int
    
    let t = generateTree depth
    
    printf "исходное дерево: "
    show t
    printfn ""
    
    printf "символ для добавления: "
    let c = Console.ReadLine().[0]

    let t2 = f t c
    printf "преобразованное дерево: "
    show t2
    0
  *)

open System

type Tree =
    | Empty
    | Node of int * Tree * Tree

let rec generateTree depth =
    if depth <= 0 then Empty
    else
        let rand = Random()
        let value = rand.Next(1, 100)
        Node(value, generateTree (depth - 1), generateTree (depth - 1))

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

let rec show tree =
    match tree with
    | Empty -> ()
    | Node(v, l, r) ->
        printf "%d " v
        show l
        show r

[<EntryPoint>]
let main args =
    printf "глубина дерева: "
    let input = Console.ReadLine()
    let depth = 
        match Int32.TryParse(input) with
        | (true, num) when num > 0 -> num
        | _ -> 
            printfn "Ошибка: введите положительное целое число"
            1
    let tree = generateTree depth
    
    printf "сгенерированное дерево: "
    show tree
    printfn ""

    let (evenSum, oddSum) = sum tree

    printf "четные = %d, нечетные = %d\n" evenSum oddSum

    if evenSum > oddSum then
        printf "больше сумма четных\n"
    elif oddSum > evenSum then
        printf "больше сумма нечетных\n"
    else
        printf "суммы равны\n"

    0