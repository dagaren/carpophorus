open Elmish
open System.Threading

let terminateModel (event:AutoResetEvent) model =
    event.Set () |> ignore

let emptyView model distpatch = ()


let readCommandsAsync sequence dispatch = 
    async {
        sequence
        |> Seq.choose ParsingUci.tryParseCommand
        |> Seq.map Engine.Command
        |> Seq.iter dispatch
    }

let sendOutput (commandStr:string) = 
    printfn "%s" commandStr

let inputSubscription (initialModel:Engine.Engine) = 
    let sub dispatch =
        readCommandsAsync IO.readInputSeq dispatch
        |> Async.Start

    Cmd.ofSub sub

let terminateEvent = new AutoResetEvent(false)

Program.mkProgram (Engine.init sendOutput) Engine.update emptyView
    |> Program.withTermination Engine.isQuitMessage (terminateModel terminateEvent)
    |> Program.withSubscription inputSubscription
    |> Program.withConsoleTrace
    |> Program.run

terminateEvent.WaitOne () |> ignore