(*
Will / @tihan & Mathias / @brandewinder
*)

open System

module ``First Problem`` =
// https://ocaml.org/learn/tutorials/99problems.html/#Writeafunctionlast39alistgt39aoptionthatreturnsthelastelementofalisteasy

    let rec last xs =
        match xs with
        | [] -> None
        | [x] -> Some(x)
        | hd::tl -> last tl

    let test1 () =
        Some("d") = last [ "a" ; "b" ; "c" ; "d" ]

    let test2 () =
        None = last []

module ``Second Problem`` =
// https://ocaml.org/learn/tutorials/99problems.html/#Findthelastbutonelastandpenultimateelementsofalisteasy

    let rec last_two xs =
        match xs with
        | [] -> None
        | [x] -> None
        | [a;b] -> Some(a,b)
        | hd::tl -> last_two tl

    // is this better? worse?
    // a bit shorter, but
    // previous version seems clearer.
    let rec last_two_simpler = function
        | [a;b] -> Some(a,b)
        | hd::tl -> last_two tl
        | [] -> None

    Some("c","d") = last_two [ "a"; "b"; "c"; "d" ]

    None = last_two [ "a" ]


#I __SOURCE_DIRECTORY__
#I @"../packages"
#r @"fscheck/lib/net45/fscheck.dll"
open FsCheck

module ``Third problem`` =
// https://ocaml.org/learn/tutorials/99problems.html/#Extractagivennumberofrandomlyselectedelementsfromalistmedium

  // WIP
  // sample from OCaml explanation:
  // rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3

  (*
  requirements:
  a,a,b,b -> 3 could be a,a,b or a,b,b but not a,a,a or b,b,b
  what if the list is too short? Should it return an option?

  3 properties should hold (at least):
  1. result should contain k elements
  2. every element in result belongs to list
  3. any item is present at most as many time in result and list
  *)

  let rng = Random()

  let remove_kth xs k =
      let rec foo (head,i) (rest:_ list) =
          match rest with
          | h:: tail when i = k ->
              head @ tail
          | h :: tail ->
              foo (h :: head, i + 1) tail
          | _ -> failwith "k is outside the bounds of xs"
      foo ([], 0) xs

  let rand_select (xs:_ list) (k:int) =
      []


  let ``selecting k should return a list of k`` xs k =
      // not true: if list is too short, should be none
      let result = rand_select xs k
      result |> List.length = k

  Check.Verbose ``selecting k should return a list of k``
