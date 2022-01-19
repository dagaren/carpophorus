module Fen

open Model
open FParsec
open Parsing

let pTurnWhite = pchar 'w' >>% White
let pTurnBlack = pchar 'b' >>% Black

let pTurn = choice [
    attempt pTurnWhite
    attempt pTurnBlack
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

let pPiece = choice [
    (pchar 'P' >>% Piece.whitePawn)
    (pchar 'R' >>% Piece.whiteRook)
    (pchar 'N' >>% Piece.whiteKnight)
    (pchar 'B' >>% Piece.whiteBishop)
    (pchar 'Q' >>% Piece.whiteQueen)
    (pchar 'K' >>% Piece.whiteKing)
    (pchar 'p' >>% Piece.blackPawn)
    (pchar 'r' >>% Piece.blackRook)
    (pchar 'n' >>% Piece.blackKnight)
    (pchar 'b' >>% Piece.blackBishop)
    (pchar 'q' >>% Piece.blackQueen)
    (pchar 'k' >>% Piece.blackKing)
]

let pSquare = pFile .>>. pRank |>> Square

let pCastlingNoRights = skipChar '-' >>% Castling.noRights

let pCaslingRight = choice [
    attempt (charReturn 'K' (White, Short))
    attempt (charReturn 'Q' (White, Long))
    attempt (charReturn 'k' (Black, Short))
    attempt (charReturn 'q' (Black, Long))
]

let foldCastlingRight currentRights (color, right) =
    currentRights |> Castling.withCastlingRight color right

let reduceCastlingRights rights = 
    rights |> List.fold foldCastlingRight Castling.noRights

let pCastlingRights = many1 pCaslingRight |>> reduceCastlingRights

let pCastling = choice [ 
    attempt pCastlingNoRights
    attempt pCastlingRights
]

type rankElement = 
| Empty of int
| Piece of Piece

let pRankelement = choice [
    attempt (pint32 |>> Empty)
    attempt (pPiece |>> Piece)
]
    
let transformRankElement rankElement = 
    match rankElement with
    | Empty num -> List.replicate num None
    | Piece piece -> [ Some piece ]

let createBoardFromRank (rank:Rank) (rankElements:rankElement list)  =
    let tElements = rankElements |> List.collect transformRankElement 

    if tElements.Length <> 8 then
        Result.Error "Invalid rank elements"
    else 
        let board = 
            tElements 
            |> List.mapi (fun index element -> element |> Option.map (fun piece -> (Square ((File.fromIndex index), rank)), piece))
            |> List.choose id
            |> Map.ofList
            |> Board
        Result.Ok board

let pBoardRank rank = many1 pRankelement |>> (createBoardFromRank rank) >>= pResult

let rankBoardsToBoard r8 r7 r6 r5 r4 r3 r2 r1 =
    [r8; r7; r6; r5; r4; r3; r2; r1]
        |> List.collect (fun (Board b) -> b |> Map.toList)
        |> Map.ofList
        |> Board

let pBoard = pipe8 
                (pBoardRank R8 .>> pchar '/')
                (pBoardRank R7 .>> pchar '/')
                (pBoardRank R6 .>> pchar '/')
                (pBoardRank R5 .>> pchar '/')
                (pBoardRank R4 .>> pchar '/')
                (pBoardRank R3 .>> pchar '/')
                (pBoardRank R2 .>> pchar '/')
                (pBoardRank R1)
                rankBoardsToBoard

let pEnPassantSquare = choice [
    attempt (pSquare |>> Some)
    attempt (pchar '-' >>% None)
]

let pHalfMoveClock = puint32
let pFullMove = puint32

let createPosition board turn castling enPassant halfMove fullMove = 
    Position.emptyPosition
    |> Position.withBoard board
    |> Position.withTurn turn
    |> Position.withCastlingRights castling
    |> Position.withEnPassantSquare enPassant
    |> Position.withHalfmoveClock halfMove
    |> Position.withFullMoveNumber fullMove

let pFen = pipe6
            (pBoard .>> spaces1)
            (pTurn .>> spaces1)
            (pCastling .>> spaces1)
            (pEnPassantSquare .>> spaces1)
            (pHalfMoveClock .>> spaces1)
            (pFullMove .>> spaces)
            createPosition

let parseFen input = 
    Parsing.runParser pFen input

let tryParseFen input = 
    Parsing.tryRunParser pFen input

module Format = 

    let formatRank rank = 
        match rank with
        | R1 -> "1"
        | R2 -> "2"
        | R3 -> "3"
        | R4 -> "4"
        | R5 -> "5"
        | R6 -> "6"
        | R7 -> "7"
        | R8 -> "8"

    let formatFile file = 
        match file with
        | A -> "a"
        | B -> "b"
        | C -> "c"
        | D -> "d"
        | E -> "e"
        | F -> "f"
        | G -> "g"
        | H -> "h"

    let formatPiece (Model.Piece (color, ptype)) = 
        match (color, ptype) with
        | White, Pawn -> "P"
        | White, Rook -> "R"
        | White, Knight -> "N"
        | White, Bishop -> "B"
        | White, Queen -> "Q"
        | White, King -> "K"
        | Black, Pawn -> "p"
        | Black, Rook -> "r"
        | Black, Knight -> "n"
        | Black, Bishop -> "b"
        | Black, Queen -> "q"
        | Black, King -> "k"

    let formatSquare (Square (file, rank)) = sprintf "%s%s" (formatFile file) (formatRank rank)

    type RankElement =
    | Piece of Piece
    | Empty of int32

    let formatRankElement re = 
        match re with
        | Piece piece -> formatPiece piece
        | Empty number -> sprintf "%d" number

    let foldRank elements pieceOption =
        match pieceOption with
        | None ->
            match elements with
            | [] -> [ Empty 1 ]
            | first :: remaining -> 
                match first with
                | Piece _ -> (Empty 1) :: elements
                | Empty number -> (Empty (number + 1)) :: remaining
        | Some piece -> (Piece piece) :: elements

    let formatBoardRank (Board squareMap) (rank:Rank) =
        File.allFiles
        |> List.map (fun file -> squareMap.TryFind (Square (file, rank)))
        |> List.fold foldRank []
        |> List.rev
        |> List.map formatRankElement
        |> List.fold (+) ""

    let formatPosition (position:Model.Position) = 
        let piecePlacement =
            Rank.allRanksRev
            |> List.map (formatBoardRank position.board)
            |> String.concat "/"

        let turn = if position.turn = Color.White then "w" else "b"
        let castling =
            let wl, ws, bl, bs = Castling.getRights position.castlingRights
           
            match Castling.getRights position.castlingRights with
            | false, false, false, false -> "-"
            | wl, ws, bl, bs ->
                let wls = if wl = true then "Q" else ""
                let wss = if ws = true then "K" else ""
                let bls = if bl = true then "q" else ""
                let bss = if bs = true then "k" else ""
                sprintf "%s%s%s%s" wss wls bss bls
                
        let enPassant = 
            match position.enPassantSquare with
            | None -> "-"
            | Some sq -> formatSquare sq
        let halfMoveClock = sprintf "%d" position.halfmoveClock
        let fullMoveNumber = sprintf "%d" position.fullMoveNumber

        let fen = sprintf "%s %s %s %s %s %s" piecePlacement turn castling enPassant halfMoveClock fullMoveNumber

        fen