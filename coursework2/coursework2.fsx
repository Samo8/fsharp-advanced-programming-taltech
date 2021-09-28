(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion, combination of functions

  ------------------------------------
  Name: Samuel Dubovec
  Tallinn University of Technology Student ID
  or Uni-ID:
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2021 under your name, into a file coursework2/coursework2.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.

  Deadline for submitting the solution is September 24 AoE, 2021.
*)

// You are given a type BibliographyItem that has the following structure:
// string list * string * (int * int) * int
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
// * "Lastname, Firstname1 Firstname2" (i.e. listing all first names after comma)
// * The second field represents the title of the publication
// * The third field represents a pair containing the starting page number and ending page number of the paper.
// * The fourth field represents the year of publication

type BibliographyItem = string list * string * (int * int) * int

// 1. Create a value bibliographyData : BibliographyItem list that contains
// at least 7 different publications on your favourite topic from https://dblp.uni-trier.de/
// Please note that you need not read the whole papers, just pick 7 papers that look interesting to you from the database.

let bibliographyData: BibliographyItem list =
    [ ([ "Bairi, Ramakrishna"
         "A, Ambha"
         "Ramakrishnan, Ganesh" ],
       "Learning to Generate Diversified Query Interpretations using Biconvex Optimization",
       (733, 739),
       2013)
      ([ "A, Sujan Reddy "; "Rudra, Bhawana" ],
       "Evaluation of Recurrent Neural Networks for Detecting Injections in API Requests",
       (936, 941),
       2021)
      ([ "Gaddam, Venkateswarlu"
         "Neella, Nagarjuna"
         "Rajanna, Konandur"
         "N, Veera Pandi"
         "Nayak, Mangalore Manjunatha" ],
       "Acoustic transducer based on ZnO nanorods",
       (481, 484),
       2017)
      ([ "N., Seenu"
         "M., Kuppan Chetty R."
         "M., Ramya M."
         "Janardhanan, Mukund Nilakantan" ],
       "Review on state-of-the-art dynamic task allocation strategies for multiple-robot systems",
       (929, 942),
       2020)
      ([ "Jeelani, Zubair"; "Qadir, Fasel" ],
       "A comparative study of cellular automata-based digital image scrambling techniques",
       (359, 375),
       2021)
      ([ "Bhawal, Chayan"
         "Qais, Imrul"
         "Pal, Debasattam" ],
       "Constrained Generalized Continuous Algebraic Riccati Equations (CGCAREs) Are Generically Unsolvable",
       (192, 197),
       2019)
      ([ "V, Akila"
         "K, Sriharshini"
         "P, Sravani"
         "D, Sravanthi"
         "Gopi, Rishita"
         "T, Sheela" ],
       "Intelligent Car Anti-Theft Face Recognition System",
       (120, 128),
       2021) ]


// 2. Make a function compareLists : string list -> string list -> int that takes two string lists and
// returns
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You need to use the support honouring the default culture for ordering strings, i.e. the built in
// compare does not work properly by default. Look at the
// documentation of the .Net comparison for strings: System.String.Compare
// If the first authors are the same
// then the precedence should be determined by the next author.
// Please note that your implementation should be recursive over the input lists.
//
// The sort order in .Net is defined using System.Globalization.CultureInfo:
// https://docs.microsoft.com/en-us/dotnet/api/system.globalization
// Please note that your solution should not force a particular sort order!

let rec compareLists (a: string list) (b: string list) : int =
    match (a, b) with
    | ([], []) -> 0
    | (x :: xs, []) -> 1
    | ([], y :: ys) -> -1
    | (x :: xs, y :: ys) ->
        let comp = System.String.Compare(x, y)

        if comp = 0 then
            compareLists xs ys
        else
            comp

let getAuthorsFromBibliographyItem (a: string list, _: string, _: (int * int), _: int) = a
let getYearFromBibliographyItem (_: string list, _: string, _: (int * int), d: int) = d
let getNumPagesFromBibliographyItem (_: string list, _: string, c: (int * int), _: int) = c


// let aa =
//     getAuthorsFromBibliographyItem (bibliographyData.Item(1))

// let bb =
//     getAuthorsFromBibliographyItem (bibliographyData.Item(0))

// printf "%d" (compareLists aa bb)

// 3. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.
// Use solution from task 3.

let compareAuthors (a: BibliographyItem) (b: BibliographyItem) : int =
    compareLists (getAuthorsFromBibliographyItem a) (getAuthorsFromBibliographyItem b)


// 4. Make a function
// compareAuthorsNumPages : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors and if the authors are
// the same then according to the number of pages in the publication.

let sumPages (pageFrom, pageTo) = pageTo - pageFrom

let getSummedPages =
    getNumPagesFromBibliographyItem >> sumPages

let compareAuthorsNumPages (a: BibliographyItem) (b: BibliographyItem) : int =
    let comp =
        compareLists (getAuthorsFromBibliographyItem a) (getAuthorsFromBibliographyItem b)

    if comp <> 0 then
        comp
    else
        compare (getSummedPages a) (getSummedPages b)


// 5. Make a function
// sortBibliographyByNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the number of pages in the
// publication in ascending order.
// If two items are at the same level in the sort order, their order should be preserved.


let sortBibliographyByNumPages (list: BibliographyItem list) = list |> Seq.sortBy (getSummedPages)

// List.sortBy
//     (fun (item: BibliographyItem) ->
//         let (pageFrom, pageTo) = getNumPagesFtomBibliographyItem item
//         pageTo - pageFrom)

sortBibliographyByNumPages bibliographyData


// 6. Make a function
// sortBibliographyByAuthorNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and number of pages in the publication in ascending order
// If two items are at the same level in the sort order, their order should be preserved.

let sortBibliographyByAuthorNumPages (list: BibliographyItem list) =
    list
    |> Seq.sortBy (fun item -> getAuthorsFromBibliographyItem item, getSummedPages item)
// list
// |> Seq.sortBy (getAuthorsFromBibliographyItem)
// |> Seq.sortBy (getNumPagesFtomBibliographyItem >> sumPages)



let getAuthorsPublications (list: BibliographyItem list) (name: string) =
    list
    |> List.filter (fun item -> List.contains name (getAuthorsFromBibliographyItem item))

// 7. Make a function
// groupByAuthor : BibliographyItem list -> (string * BibliographyItem list) list
// where the return list contains pairs where the first element is the name of a single
// author and the second element a list of bibliography items that the author has co-authored.

let groupByAuthor (list: BibliographyItem list) =
    list
    |> List.collect (getAuthorsFromBibliographyItem)
    |> List.map (fun name -> (name, getAuthorsPublications list name))
    |> List.distinct


// let x = groupByAuthor bibliographyData
// printfn "%A" (x.Head)
