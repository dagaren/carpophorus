module Engine

open Elmish
open UciProtocol.Input

type Engine = {
    name: string
    author: string
    position: Model.Position option
    outputFunc: string->unit
}

type Msg = Command of UciProtocol.Input.Command

let createEngine outputFunc = {
    name = "Carpophorus"
    author = "David Garcinuño"
    position = None
    outputFunc = outputFunc
} 

let init outputFunc = 
    fun () ->
        createEngine outputFunc
        , Cmd.none

let handlePositionCommand (PositionCommand (position, moveList))  model =
    { model with position = Some position },
    Cmd.none 

let handleUciCommand model = model, Cmd.none

let handleDebugCommand command model = model, Cmd.none

let handleIsReadyCommand model = model, Cmd.none

let handleSetOptionCommand command model = model, Cmd.none

let handleRegisterCommand command model = model, Cmd.none

let handleUciNewGameCommand model = model, Cmd.none

let handleGoCommand command model = model, Cmd.none

let handleStopCommand model = model, Cmd.none

let handlePonderhitCommand model = model, Cmd.none

let handleQuitCommand model = model, Cmd.none

let executeUciCommand uciCommand model = 
    match uciCommand with
    | Position command -> handlePositionCommand command model
    | Uci -> handleUciCommand model
    | Debug command -> handleDebugCommand command model
    | IsReady -> handleIsReadyCommand model
    | SetOption command -> handleSetOptionCommand command model
    | Register command -> handleRegisterCommand command model
    | UciNewGame -> handleUciNewGameCommand model
    | Go command -> handleGoCommand command model
    | Stop -> handleStopCommand model
    | Ponderhit -> handlePonderhitCommand model
    | Quit -> handleQuitCommand model

let update msg model = 
    match msg with
       | Command command ->
           executeUciCommand command model

let isQuitMessage msg = 
    match msg with
    | Command UciProtocol.Input.Quit -> true
    | _ -> false