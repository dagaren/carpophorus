module Parsing

open FParsec

let runParser parser str = 
    match run  parser str with
    | Success(result, _, _)   -> Result.Ok result
    | Failure(errorMsg, _, _) -> Result.Error (sprintf "Invalid input: %s" errorMsg)

let tryRunParser parser str = 
    match run  parser str with
    | Success(result, _, _)   -> Some result
    | Failure(errorMsg, _, _) -> 
        printf "Invalid input: %s" errorMsg
        None

let pipe6 p1 p2 p3 p4 p5 p6 f = 
    pipe3 p1 p2 (tuple4 p3 p4 p5 p6) (fun x1 x2 (x3, x4, x5, x6) -> f x1 x2 x3 x4 x5 x6)

let pipe8 p1 p2 p3 p4 p5 p6 p7 p8 f = 
    pipe5 p1 p2 p3 p4 (tuple4 p5 p6 p7 p8) (fun x1 x2 x3 x4 (x5, x6, x7, x8) -> f x1 x2 x3 x4 x5 x6 x7 x8)

let pResult result = 
    match result with 
    | Result.Ok x -> preturn x
    | Result.Error error -> fail error