(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: FSharpON

  ------------------------------------
  Name: Samuel Dubovec
  Tallinn University of Technology Student ID
  or Uni-ID: 214302IV
  ------------------------------------

  Answer the questions below. Your answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework4/coursework4.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect it will not be graded.

  Your solution must not use mutable state and imperative features
  like for loops.

*)


(*

The ECMA-404 standard specifies a textual syntax for structured data
interchange.

The specification is available here:
https://www.ecma-international.org/wp-content/uploads/ECMA-404_2nd_edition_december_2017.pdf

The goal of this coursework is to develop a partial implementation of
this specification. In particular, our first goal is to define in F#
the datatype(s) suitable for representing the abstract syntax tree of
this data interchange format. The second goal is to define some
operations on this representation.

*)

// We have the following type alias.

type Name = string

//// Task 1 ////

// Define the type `Ecma` for representing the possible values of the
// ECMA-404 interchange format.
//
// The type must satisfy the constraint `equality`.


type Ecma =
    | None
    | Bool of bool
    | Number of float
    | Text of string
    | List of list<Ecma>
    | Object of (Name * Ecma) list


// Define the following functions for creating ECMA-404
// representations of the given data.

(*
Define the function

  mkObject : unit -> Ecma

that creates a representation for an empty object structure.
*)


let mkObject () : Ecma = Object([])



(*
Define the function

  mkNumber : float -> Ecma

that creates a representation for the given floating-point number.
*)

let mkNumber value = Number value


(*
Define the function

  mkBool : bool -> Ecma

that creates a representation for the given Boolean value.
*)

let mkBool value = Bool value


(*
Define the function

  mkString : string -> Ecma

that creates a representation for the given string value.
*)

let mkString value = Text value


(*
Define the function

 mkArray : Ecma list -> Ecma

that creates a representation for an array whose elements are
represented by the given list of `Ecma` values.
*)

let mkArray list : Ecma = List list


(*
Define the function

  mkNull : unit -> Ecma

that creates a representation of the ECMA-404 `null` value.
*)

let mkNull () : Ecma = None




//// Task 2 ////

// Define the function
//
//   addNameValue : Name * Ecma -> Ecma -> Ecma
//
// so that
//
//   addNameValue (n, v) e
//
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an object representation
//
// - a representation for the object e extended with the name-value
//   pair (n, v), otherwise.

let addNameValue (n: Name, v: Ecma) (e: Ecma) : Ecma =
    match e with
    | Object o -> Object(o @ [ n, v ])
    | _ -> e


// Define the function
//
//   addValue : Ecma -> Ecma -> Ecma
//
// so that
//
//   addValue v e
//
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an array representation
//
// - a representation for the array e with the value v added as the last
//   element, otherwise.

let addValue (v: Ecma) (e: Ecma) : Ecma =
    match e with
    | List list -> List(List.append list [ v ])
    | _ -> e




//// Task 3 ////

// Define the function
//
//   countValues : Ecma -> int
//
// that counts the number of ECMA values in the given representation.
//
// Keep in mind that both objects and arrays are themselves values and
// may contain other values inside.
//
// Furthermore, the following should hold:
//
//   1 + countValues e <= countValues (addValue v e)             // if e is an array representation
//
//   1 + countValues e <= countValues (addNameValue (n, v) e)    // if e is an object representation

let rec countValues (e: Ecma) : int =
    match e with
    | List list ->
        list
        |> List.fold (fun v item -> v + countValues item) 1
    | Object o ->
        o
        |> List.fold (fun v item -> v + countValues (snd item)) 1
    | _ -> 1

// let person =
//     [ ("name", Text "Samuel")
//       ("age", Number(Float 2.3)) ]

// let cars =
//     [ "Skoda", None
//       "VW ", Text "Golf"
//       "Random", Object person
//       "Lamborghini", Text "Aventador"
//       "Mercedes", Text "G"
//       "BMW", List([ Text "M5"; Text "X6" ]) ]

// printfn "%A" cars

// let e: Ecma = Object(cars)

// let emptyListEcma = List([])
// let emptyMapEcma = Object(Map.empty)


// printfn "%A" (countValues emptyMapEcma)

// let emptyEcma1 =
//     addNameValue ("Nieco", Number(Int 3)) (emptyMapEcma)

// printfn "%A" (countValues emptyEcma1)

// let emptyEcma2 =
//     addNameValue ("Nieco", Number(Int 3)) (emptyEcma1)

// printfn "%A" (countValues emptyEcma2)

//// Task 4 ////

type Path = Name list


// Define the function
//
//   listPaths : Ecma -> Path list
//
// that computes the full path for all the values in the given ECMA
// representation.
//
// A path is just a list of names that take us from the root of the
// representation to a particular value.
//
// For arrays, we consider the same path to take us to the array and to
// all of the elements in the array. Thus, for an array, we include the
// path to it and its elements only once in the result.
//
// If `e : Ecma` represents the following structure
//
//   {
//     "abc" : false,
//     "xs"  : [ { "a" : "a" }, 1.0, true, { "b" : "b" }, false ],
//     "xyz" : { "a" : 1.0,
//               "b" : { "b" : "b" } },
//     "ws"  : [ false ]
//   }
//
// then  `listPaths e` should result in
//
//   [
//     [];
//     ["abc"];
//     ["xs"];
//     ["xs"; "a"];
//     ["xs"; "b"];
//     ["xyz"];
//     ["xyz"; "a"];
//     ["xyz"; "b"];
//     ["xyz"; "b"; "b"];
//     ["ws"]
//   ]
//
// The ordering of paths in the result list matters:
// - paths to (sub)values in an array respect the order of elements in
//   the array
//
// - paths to values in an object respect the order in which the values
//   were added to the object (most recently added appears last).
//
// Note that the empty list denotes the path to the root object.


let rec listPaths (ecma: Ecma): Path list =
    let rec paths (e: Ecma) (path: Path): Path list =
        match e with
        | Object o -> 
            o |> List.collect(fun (name, value) -> 
              let newPath = path @ [name]
              [newPath] @ paths value newPath)
        | List l -> l |> List.collect(fun x -> paths x path)
        | _ -> []

    [] :: paths ecma []


// let capitals =
//     [ "x", Bool true
//       "abc", Bool false
//       "xs",
//       List(
//           [ Object([ "a", Text "a" ])
//             Number 1.0
//             Bool true
//             Object([ "b", Text "b" ])
//             Bool false ]
//       )
//       "xyz",
//       Object(
//           [ "a", Number 1.0
//             "b", Object([ "b", Text "b" ]) ]
//       )
//       "ws", List([ Bool false ]) ]

// let e: Ecma = Object(capitals)

// printfn "%A" (listPaths e)


//// Task 5 ////

// Define the function
//
//   show : Ecma -> string
//
// that computes a string representation of the given ECMA representation
// in such a way that the ordering requirements from the previous task are
// respected.
//
// The result should not contain any whitespace except when this
// whitespace was part of a name or a string value.


let rec show (ecma: Ecma) =
    match ecma with
    | Object o -> "{" + (String.concat "," (o |> List.map(fun (name, e) -> show (Text name) + ":" + show e))) + "}"
    | List l -> "[" + String.concat "," (l |> List.map(fun item -> show item)) + "]"
    | Bool b -> b.ToString().ToLower()
    | Number n -> n.ToString()
    | Text t -> "\"" + t + "\""
    | None -> "null"

// let x = Object([ ("text", List([ Number 1.0; Bool false ])); ("text2222", List([ Number 1.0; Bool false ])); ("text333", List([ Number 1.0; Bool false ])); ("xx", Object([])) ])
// let x = List([Number 1.0; Bool false; Number 3.0])
   

// printfn "%s" (show x)

// printfn
//     "%s"
//     (show (
//         List(
//             [ Bool false
//               Text "fasf"
//               Number 4.5
//               Number 1.0 ]
//         )
//     ))

// let obj =
//     Object(
//         [ ("meno", Text "Samuel")
//           ("priezvisko", Text "Dubovec")
//           ("2_nohy", Bool true)
//           ("xx", Object([]))
//           ("y", List([])) ]
//     )

// let listik = List([ Text "Samuel"; Bool true ])

// printfn "%s" (show obj)
// printfn "%s" (show (Object capitals))

// printfn "%s" "aa"


//// Task 6 ////

//   {
//     "abc" : false,
//     "xs"  : [ { "a" : "a" }, 1.0, true, { "b" : "b" }, false ],
//     "xyz" : { "a" : 1.0,
//               "b" : { "b" : "b" } },
//     "ws"  : [ false ]
//   }
//
// then  `listPaths e` should result in
//
//   [
//     [];
//     ["abc"];
//     ["xs"];
//     ["xs"; "a"];
//     ["xs"; "b"];
//     ["xyz"];
//     ["xyz"; "a"];
//     ["xyz"; "b"];
//     ["xyz"; "b"; "b"];
//     ["ws"]
//   ]

// Define the function
//
//   delete : Path list -> Ecma -> Ecma
//
// so that
//
//   delete ps e
//
// evaluates to a representation `e'` that is otherwise the same as `e` but
// all name-value pairs with paths in the path list `ps` have been removed.
//
// When the user attempts to delete the root object, delete should throw
// an exception. Hint: use `failwith` in the appropriate case.



let delete (list: Path list) (e: Ecma) : Ecma = e

// let listPathCapital = [ [ "abc" ]; [ "xs" ] ]
// let listPathCapital = [ [ "abc" ] ]

// printfn "%A" (Object capitals)
// printfn "%A" (delete listPathCapital (Object capitals))

// [ []
//   [ "abc" ]
//   [ "xs" ]
//   [ "xs"; "a" ]
//   [ "xs"; "b" ]
//   [ "xyz" ]
//   [ "xyz"; "a" ]
//   [ "xyz"; "b" ]
//   [ "xyz"; "b"; "b" ]
//   [ "ws" ] ]


//// Task 7 ////

// Define the function
//
//   withPath : Path list -> Ecma -> Ecma list
//
// so that
//
//   withPath ps e
//
// evaluates to a list of object representations consisting of those
// objects in `e` that are represented by a path from the list `ps`.
//
// The result list must respect the ordering requirements from Task 4.

let withPath (list: Path list) (e: Ecma) : Ecma list = []
