open System

module ``First Problem`` =

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

    let rec last_two xs =
        match xs with
        | [] -> None
        | [x] -> None
        | [a;b] -> Some(a,b)
        | hd::tl -> last_two tl             

    Some("c","d") = last_two [ "a"; "b"; "c"; "d" ]

    None = last_two [ "a" ]