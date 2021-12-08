(*
  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 8: Sequences, laziness and computation expressions

  ------------------------------------------------------------------------------
  Name: Samuel Dubovec
  Student ID: 214302IV
  ------------------------------------------------------------------------------


  Answer the questions below. Your answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework8.fsx in directory coursework8.


  The deadline for completing the above procedure is Sunday, December 5, 2021.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)




(*
  Task 1: Pascal's triangle

             1
            1 1
           1 2 1
          1 3 3 1
         1 4 6 4 1
        ...........
       .............
      ............... 
  

  Define the function

    next : int list -> int list

  that, given a row of the triangle, computes the next row. The
  function List.windowed may be useful here.


  Define the sequence

    triangle : int list seq

  which consists of the rows of Pascal's triangle (represented as int
  list). Do not use sequence expressions. Define this using
  Seq.unfold.


  Define the function

    evens : int -> int list

  so that

    evens n

  evaluates to a list (of length n) consisting of the sums of elements
  in the first n rows of Pascal's triangle that have an even number of
  elements.

*)

// let next (list: int list) = 1::(((List.rev (0::list) |> List.windowed 2)) |> List.collect(fun item -> [List.sum item]))

let next (arr:int list) : int list =
  match arr |> List.isEmpty with
  | true -> [1]
  | _ -> (arr |> List.windowed 2 |> List.fold (fun state item -> (List.sum item)::state) [1] |> List.rev) @ [1]


// let resultNext = next [1;3;3;1]
// let resultNext = next(next(next(next(next [1]))))

// printf "%A" resultNext

let triangle: int list seq = Seq.unfold(fun state -> Some(state, next state)) (next [])


// printfn "%A" triangle

// triangle |> Seq.take 20

let evens (n: int): int list = 
  triangle 
  |> Seq.filter(fun item -> item.Length % 2 = 0)
  |> Seq.map Seq.sum
  |> Seq.take n
  |> Seq.toList




(*
  Task 2

  Define the function

    generate : 'a list -> ('a list -> 'a) -> 'a seq

  so that

    generate xs f

  evaluates to a sequence consisting of the elements in xs followed by
  elements computed by the function f.

  More precisely, if List.length xs = n, then s_i (the i-th element in
  the sequence) is

  * the i-th element of the list xs   if i < n
  * f [s_{i - n}; ... ; s_{i - 1}]     otherwise

  Note that f must be applied to lists of same length as xs.

  You may assume that xs is not empty.

  Define this using sequence expressions.

  Make sure that the calculation of an element of the sequence uses
  the function f at most once.

  The function Seq.cache may be useful here.

*)

// let ss = seq {
  //   xs
  //   // for i in 0..xs.Length-1 do
  //   //   f xs
  // }
  // let ss = xs |> List.toSeq

  // let rec loop value = seq {
  //   // if i < xs.Length then
  //   //   yield value
  //   yield! loop 
  // }
  // loop xs
  // seq { xs.Head }
  // let ss = seq {
  //   yield! xs
  //   yield ((xs |> List.toSeq) |> Seq.windowed 3)
  // }
  // let z = Seq.windowed 3 ss
  // seq {xs.Head}

let generate (xs: 'a list) (f: 'a list -> 'a): 'a seq = 
  let rec innerGenerate (list: 'a list): 'a seq = 
    let next = f list
    
    seq {
      next
      yield! (innerGenerate (list.Tail @ [next]))
    }
  seq {
    yield! xs
    yield! innerGenerate xs
  }


// let genResult = generate [1; 2] List.sum
// printfn "%A" (genResult |> Seq.item 4) 


// let generate (xs: 'a list) (f: 'a list -> 'a): 'a seq = 
//   let xsLength = xs.Length
//   let n = xsLength * 2
//   let newSeq =  xs |> List.toSeq |> Seq.cache
//   seq { for i in 0..n-1 do 
//           match i < xsLength with 
//           | true -> xs.Item i 
//           | _ -> 
//             printfn "%d" i
//             printfn "%A" (newSeq |> Seq.take (i - xsLength))
//             f ( newSeq |> Seq.take (i - xsLength) |> Seq.toList)
//   }

// let genResult = generate [1;2;3] (fun list -> list.Head)

// printfn "%A" (genResult |> Seq.toList)

// let generate (xs: 'a list) (f: 'a list -> 'a): 'a seq = 

(*
  Task 3: Longest common subsequence
  
  We have two arrays, xs and ys, and we wish to find the length of the
  longest common subsequence in these two arrays.
  
  Example:
  
  - xs = [| 1; 2; 3; 4 |]
  - ys = [| 5; 1; 6; 4 |]
  
  Length of the longest common subsequence is 2.
  
  This can be solved using dynamic programming.
  
  Let D be a two-dimensional array that holds the results of the
  subproblems:
  - D[i, j] is the length of the lcs of xs[0 .. i - 1] and ys[0 .. j - 1].
  
  Solving the subproblems:
  - if xs[i - 1] = ys[j - 1] then we follow one subproblem (shorten both sequences):
      D[i, j] = D[i - 1, j - 1] + 1
  
  - otherwise we take the maximum of two subproblems:
      D[i, j] = max D[i - 1, j] D[i, j - 1]
  
  - base cases:
      D[i, 0] = D[0, j] = 0
  
  
  Observation: it is not necessary to fill the entire table D to
  calculate D[i, j].
  
  If we decide to fill only those parts of the table that are necessary
  to compute D[i, j], then we need to be careful to not use the values
  in the unfilled parts in our calculation.
  
  However, we can use lazy values instead and let the runtime figure out
  which entries in the table and in which order need to be calculated.
  
  Define the function
  
    lcs : ((int * int) -> unit) -> 'a [] -> 'a [] -> Lazy<int> [,]
  
  so that
  
    lcs m xs ys
  
  evaluates to the table D for xs and ys except that the entries in the
  table are lazy. An entry in the table is computed only when we ask for
  the value (of the Lazy<int>) or the computation of another entry
  requires the value of this entry.
  
  The function m must be applied to (i, j) when the entry D[i, j] is
  actually computed. For example, you can use printfn as m to make the
  order of calculations visible.

*)

let lcs (m: (int * int) -> unit) (xs: 'a []) (ys: 'a []): Lazy<int> [,] =
  let xsLength = xs.Length + 1
  let ysLength = ys.Length + 1
  let table = Array2D.zeroCreate<Lazy<int>> xsLength ysLength
  table.[*, 0] <- [|for i in 1..xsLength do (lazy (m (i-1, 0) ; 0))|]
  table.[0, *] <- [|for j in 1..ysLength do (lazy (m (0, j-1) ; 0))|]
  
  xs |> Array.mapi(
      fun i item -> ys |> Array.mapi(fun j item2 ->
      match compare item item2 with
      | 0 -> 
        table.[i+1, j+1] <- lazy (
          m (i+1, j+1);
         (table.[i, j].Value + 1)
        )
      | _ -> 
        table.[i+1, j+1] <- lazy (
          m (i+1, j+1);
          (max table.[i+1, j].Value table.[i, j+1].Value)
        )
      ))
  |> ignore
  table
  

// let mFunc (a, b) = printfn "Nazdar %A" (a,b) 
// let lcsResult = lcs mFunc [|1;2;3;4|] [|5;1;6;4|]
// let lcsResult = lcs mFunc [|1;2|] [|1;4;2|]
// lcsResult.[0,0].Value
// lcsResult.[0,1].Value
// lcsResult.[0,2].Value
// lcsResult.[0,3].Value
// lcsResult.[1,0].Value
// lcsResult.[1,1].Value
// lcsResult.[1,2].Value
// lcsResult.[1,3].Value
// lcsResult.[2,0].Value
// lcsResult.[2,1].Value
// lcsResult.[2,2].Value
// lcsResult.[2,3].Value

// lcsResult.[4,4].Value

// let lcsResult = lcs mFunc [|'b'; 'd'|] [|'a';'b';'c';'d'|]

// lcsResult.[1,1].Value

// table.[1,*] <- (lazy 1)


// table.[5,0] <- lazy (max table.[1,0].Value table.[2,0].Value)


// let table2 = table |> Array2D.set

// let zzz = Array.empty<int>

(*
  Task 4:

  A function from a type 'env to a type 'a can be seen as a computation that
  computes a value of type 'a based on an environment of type 'env. We call such
  a computation a reader computation, since compared to ordinary computations,
  it can read the given environment. Below you find the following:

    • the definition of a builder that lets you express reader computations
      using computation expressions

    • the definition of a reader computation ask : 'env -> 'env that returns the
      environment

    • the definition of a function runReader : ('env -> 'a) -> 'env -> 'a that
      runs a reader computation on a given environment

    • the definition of a type Expr of arithmetic expressions

  Implement a function eval : Expr -> Map<string, int> -> int that evaluates
  an expression using an environment which maps identifiers to values.
  
  NB! Use computation expressions for reader computations in your implementation.
  
  Note that partially applying eval to just an expression will yield a function of
  type map <string, int> -> int, which can be considered a reader computation.
  This observation is the key to using computation expressions.

  The expressions are a simplified subset based on
  Section 18.2.1 of the F# 4.1 specification:
  https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf

*)

type ReaderBuilder () =
  member this.Bind   (reader, f) = fun env -> f (reader env) env
  member this.Return x           = fun _   -> x

let reader = new ReaderBuilder ()

let ask = id

let runReader = (<|)

type Expr =
  | Const  of int          // constant
  | Ident  of string       // identifier
  | Neg    of Expr         // unary negation, e.g. -1
  | Sum    of Expr * Expr  // sum 
  | Diff   of Expr * Expr  // difference
  | Prod   of Expr * Expr  // product
  | Div    of Expr * Expr  // division
  | DivRem of Expr * Expr  // division remainder as in 1 % 2 = 1
  | Let    of string * Expr * Expr // let expression, the string is the identifier.


let eval (e:Expr) : (Map<string, int> -> int) = failwith "not yet implemented"

// //Example:
// //keeping in mind the expression: let a = 5 in (a + 1) * 6
// let expr = Let ("a",Const 5, Prod(Sum(Ident("a"),Const 1),Const 6))
// eval expr Map.empty<string,int>
// should return 36     
