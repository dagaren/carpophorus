module ParsingUci

open FParsec
open UciProtocol.Input
open Model

let pOn = pstringCI "on" >>% true
let pOff = pstringCI "off" >>% false

let pOption = choice [
    attempt pOn
    attempt pOff
]

let pUci = stringCIReturn "uci" Uci
let pDebug = pstringCI "debug" >>. spaces1 >>. pOption |>> DebugCommand |>> Debug
let pIsReady = stringCIReturn "isready" IsReady
let pRegister = pstringCI "register " >>. (many1 anyChar) |>> string |>> RegisterCommand |>> Register
let pUciNewGame = stringCIReturn "ucinewgame" UciNewGame

let isNotWhitespace char = 
    char <> ' ' && char <> '\n' && char <> '\r' && char <> '\t'

let pStartPosition = stringCIReturn "startpos" Model.Position.initialPosition
let pFen = pstring "fen" >>. spaces1 >>. Fen.pFen
let pPos = choice [
    attempt pStartPosition
    attempt pFen
]

let pRank = choice [
    (pchar '1' >>% R1)
    (pchar '2' >>% R2)
    (pchar '3' >>% R3)
    (pchar '4' >>% R4)
    (pchar '5' >>% R5)
    (pchar '6' >>% R6)
    (pchar '7' >>% R7)
    (pchar '8' >>% R8)
]

let pFile =  choice [
    (pchar 'a' >>% A)
    (pchar 'b' >>% B)
    (pchar 'c' >>% C)
    (pchar 'd' >>% D)
    (pchar 'e' >>% E)
    (pchar 'f' >>% F)
    (pchar 'g' >>% G)
    (pchar 'h' >>% H)
]

let pPieceType = choice [
    (pchar 'r' >>% PieceType.Rook)
    (pchar 'q' >>% PieceType.Queen)
    (pchar 'n' >>% PieceType.Knight)
    (pchar 'b' >>% PieceType.Bishop)
]

let pSquare = pFile .>>. pRank |>> Square

let pMove = choice [
    attempt (pipe3 pSquare pSquare pPieceType Move.createPromotion)
    attempt (pipe2 pSquare pSquare Move.create)
]

let pMoves1 = sepBy1 pMove spaces1
let pMoves = sepBy pMove spaces1

let pPositionMoves = pstringCI "moves" >>. choice [
                                                attempt (spaces1 >>. pMoves)
                                                attempt (spaces >>% [])
                                            ]

let pPosition = pstringCI "position" >>. spaces1 >>. pPos .>> spaces1 .>>. pPositionMoves |>> PositionCommand |>> Position

let pSearchMoves = pstring "searchmoves" >>. spaces1 >>. pMoves1 |>> SearchMoves
let pPonder = stringCIReturn "ponder" Ponder
let pWhiteTime = pstringCI "wtime" >>. spaces1 >>. pint32 |>> WhiteTime
let pBlackTime = pstringCI "btime" >>. spaces1 >>. pint32 |>> BlackTime
let pWhiteIncrement = pstringCI "winc" >>. spaces1 >>. pint32 |>> WhiteIncrement
let pBlackIncrement = pstringCI "binc" >>. spaces1 >>. pint32 |>> BlackIncrement
let pMovesToGo = pstringCI "movestogo" >>. spaces1 >>. pint32 |>> MovesToGo
let pDepth = pstringCI "depth" >>. spaces1 >>. pint32 |>> Depth
let pNodes = pstringCI "nodes" >>. spaces1 >>. pint32 |>> Nodes
let pMate = pstringCI "mate" >>. spaces1 >>. pint32 |>> Mate
let pMoveTime = pstringCI "movetime" >>. spaces1 >>. pint32 |>> MoveTime
let pInfinite = stringCIReturn "infinite" Infinite

let pGoSubCommand = choice [
    attempt pSearchMoves
    attempt pPonder
    attempt pWhiteTime
    attempt pBlackTime
    attempt pWhiteIncrement
    attempt pBlackIncrement
    attempt pMovesToGo
    attempt pDepth
    attempt pNodes
    attempt pMate
    attempt pMoveTime
    attempt pInfinite
]

let pGoSubCommands = sepBy1 pGoSubCommand spaces1

let pGo = choice [
    attempt (pstringCI "go" >>. spaces1 >>. pGoSubCommands |>> GoCommand |>> Go )
    attempt (pstringCI "go" >>% GoCommand [] |>> Go)
]

let pStop = stringCIReturn "stop" Stop

let pPonderhit = stringCIReturn "ponderhit" Ponderhit

let pQuit = stringCIReturn "quit" Quit

let pUciCommand = choice [
    attempt pDebug
    attempt pIsReady
    attempt pRegister
    attempt pUciNewGame
    attempt pPosition
    attempt pGo
    attempt pStop
    attempt pPonderhit
    attempt pUci
    attempt pQuit ] .>> spaces .>> eof

let parseCommand input = 
    Parsing.runParser pUciCommand input

let tryParseCommand input = 
    Parsing.tryRunParser pUciCommand input
