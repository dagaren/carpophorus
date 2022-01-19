module IO

let sendOutput commandText = 
    printfn "%s" commandText

let readInput () = 
    System.Console.ReadLine()


let readInputSeq = seq {
    while true do
        yield (readInput ())
}



