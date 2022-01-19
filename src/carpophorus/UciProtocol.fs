module UciProtocol

module Input = 

    type GoSubCommand =
    | SearchMoves of Model.Move list
    | Ponder
    | WhiteTime of int
    | BlackTime of int
    | WhiteIncrement of int
    | BlackIncrement of int
    | MovesToGo of int
    | Depth of int
    | Nodes of int
    | Mate of int
    | MoveTime of int
    | Infinite

    type DebugCommand = DebugCommand of bool

    type SetOptionCommand = SetOptionCommand of string * string option

    type RegisterCommand = RegisterCommand of string

    type PositionCommand = PositionCommand of Model.Position * Model.Move list

    type GoCommand = GoCommand of GoSubCommand list

    type Command = 
    | Uci
    | Debug of DebugCommand
    | IsReady
    | SetOption of SetOptionCommand
    | Register of RegisterCommand
    | UciNewGame
    | Position of PositionCommand
    | Go of GoCommand
    | Stop
    | Ponderhit
    | Quit

module Output =
    type IdCommand = 
    | Name of string
    | Author of string

    type OptionType = 
    | Check
    | Spin
    | Combo
    | Button
    | String

    type CopyProtectionState = 
    | Checking
    | Ok
    | Error

    type Score = 
    | Centipawn of int
    | Mate of int
    | LowerBound
    | UpperBound

    type InfoCommand =
    | Depth of int
    | SelectiveDepth of int
    | Time of int
    | Nodes of int
    | PrincipalVariation of string list
    | MultiPV of int
    | Score of Score
    | CurrentMove of string
    | CurrentMoveNumber of int
    | HashFull of int
    | NodesPerSecond of int
    | EndgameTableHits of int
    | CpuLoad of int
    | Text of string
    | Refutation of string list
    | CurrentLine of int option * string list

    type Command = 
    | Id of IdCommand
    | UciOk
    | ReadyOk
    | Bestmove of string * string option
    | Copyprotection of CopyProtectionState
    | Registration
    | Info of InfoCommand list