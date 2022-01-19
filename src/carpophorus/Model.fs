module Model

type Color = 
| White
| Black

type PieceType = 
| King
| Queen
| Bishop
| Knight
| Rook
| Pawn

type File =
| A
| B
| C
| D
| E
| F
| G
| H

type Rank =
| R1
| R2
| R3
| R4
| R5
| R6
| R7
| R8

type CastlingRight = {
    long: bool
    short: bool
}

type CastlingRights = {
    white: CastlingRight
    black: CastlingRight
}

type CastlingRightType = 
| Long
| Short

type Square = Square of File * Rank

type Piece = Piece of Color * PieceType

type Board = Board of Map<Square,Piece>

type Position = {
    board: Board
    turn: Color
    castlingRights: CastlingRights
    enPassantSquare: Square option
    halfmoveClock: uint
    fullMoveNumber: uint
}

type Move = {
    source: Square
    destination: Square
    promotion: PieceType option
}

module File = 
    let allFiles = [A; B; C; D; E; F; G; H]
    let allFilesRev = allFiles |> List.rev

    let tryFromIndex index = 
        match index with 
        | 0 -> Some A
        | 1 -> Some B
        | 2 -> Some C
        | 3 -> Some D
        | 4 -> Some E
        | 5 -> Some F
        | 6 -> Some G
        | 7 -> Some H
        | _ -> None
    let fromIndex index = 
        match index with 
        | 0 -> A
        | 1 -> B
        | 2 -> C
        | 3 -> D
        | 4 -> E
        | 5 -> F
        | 6 -> G
        | 7 -> H
        | _ -> failwith "Invalid file index"

module Rank = 
    let allRanks = [R1; R2; R3; R4; R5; R6; R7; R8]
    let allRanksRev = allRanks |> List.rev

    let tryFromNumber number = 
        match number with 
        | 1 -> Some R1
        | 2 -> Some R2
        | 3 -> Some R3
        | 4 -> Some R4
        | 5 -> Some R5
        | 6 -> Some R6
        | 7 -> Some R7
        | 8 -> Some R8
        | _ -> None

    let fromNumber number = 
        match number with 
        | 1 -> R1
        | 2 -> R2
        | 3 -> R3
        | 4 -> R4
        | 5 -> R5
        | 6 -> R6
        | 7 -> R7
        | 8 -> R8
        | _ -> failwith "Invalid rank number"


module Castling =
    let noRights = {
        white = { long = false; short = false }
        black = { long = false; short = false }
    }

    let withCastlingRight color right rightsSet = 
        match color, right with
        | White, Long -> 
            { rightsSet with white = { rightsSet.white with long = true }}
        | White, Short ->
            { rightsSet with white = { rightsSet.white with short = true }}
        | Black, Long -> 
            { rightsSet with black = { rightsSet.black with long = true }}
        | Black, Short ->
            { rightsSet with black = { rightsSet.white with short = true }}
    let withoutCastlingRight color right rightsSet = 
        match color, right with
        | White, Long -> 
            { rightsSet with white = { rightsSet.white with long = false }}
        | White, Short ->
            { rightsSet with white = { rightsSet.white with short = false }}
        | Black, Long -> 
            { rightsSet with black = { rightsSet.black with long = false }}
        | Black, Short ->
            { rightsSet with black = { rightsSet.white with short = false }}

    let hasCastlingRight color right rightsSet = 
        match color, right with
        | White, Long -> rightsSet.white.long
        | White, Short -> rightsSet.white.short
        | Black, Long -> rightsSet.black.long
        | Black, Short -> rightsSet.black.short

    let getRights rightsSet =
        rightsSet.white.long, rightsSet.white.short, rightsSet.black.long,rightsSet.black.short

module Square = 
    let (a1, b1, c1, d1) = (Square (A,R1), Square (B,R1), Square (C,R1), Square (D,R1))
    let (a2, b2, c2, d2) = (Square (A,R2), Square (B,R2), Square (C,R2), Square (D,R2))
    let (a3, b3, c3, d3) = (Square (A,R3), Square (B,R3), Square (C,R3), Square (D,R3))
    let (a4, b4, c4, d4) = (Square (A,R4), Square (B,R4), Square (C,R4), Square (D,R4))
    let (a5, b5, c5, d5) = (Square (A,R5), Square (B,R5), Square (C,R5), Square (D,R5))
    let (a6, b6, c6, d6) = (Square (A,R6), Square (B,R6), Square (C,R6), Square (D,R6))
    let (a7, b7, c7, d7) = (Square (A,R7), Square (B,R7), Square (C,R7), Square (D,R7))
    let (a8, b8, c8, d8) = (Square (A,R8), Square (B,R8), Square (C,R8), Square (D,R8))
    let (e1, f1, g1, h1) = (Square (E,R1), Square (F,R1), Square (G,R1), Square (H,R1))
    let (e2, f2, g2, h2) = (Square (E,R2), Square (F,R2), Square (G,R2), Square (H,R2))
    let (e3, f3, g3, h3) = (Square (E,R3), Square (F,R3), Square (G,R3), Square (H,R3))
    let (e4, f4, g4, h4) = (Square (E,R4), Square (F,R4), Square (G,R4), Square (H,R4))
    let (e5, f5, g5, h5) = (Square (E,R5), Square (F,R5), Square (G,R5), Square (H,R5))
    let (e6, f6, g6, h6) = (Square (E,R6), Square (F,R6), Square (G,R6), Square (H,R6))
    let (e7, f7, g7, h7) = (Square (E,R7), Square (F,R7), Square (G,R7), Square (H,R7))
    let (e8, f8, g8, h8) = (Square (E,R8), Square (F,R8), Square (G,R8), Square (H,R8))

module Board = 
    let addPiece square piece (Board squareMap) = squareMap |> Map.add square piece |> Board

    let emptyBoard = Board (Map.empty<Square,Piece>)

module Piece = 
    let whitePawn = Piece (White, Pawn)
    let whiteRook = Piece (White, Rook)
    let whiteKnight = Piece (White, Knight)
    let whiteBishop = Piece (White, Bishop)
    let whiteQueen = Piece (White, Queen)
    let whiteKing = Piece (White, King)
    let blackPawn = Piece (Black, Pawn)
    let blackRook = Piece (Black, Rook)
    let blackKnight = Piece (Black, Knight)
    let blackBishop = Piece (Black, Bishop)
    let blackQueen = Piece (Black, Queen)
    let blackKing = Piece (Black, King)

module Position =

    let emptyPosition = {
        board = Board.emptyBoard
        turn = Color.White
        castlingRights = Castling.noRights
        enPassantSquare = None
        halfmoveClock = 0u
        fullMoveNumber = 1u
    }

    let withBoard board position = { position with board = board }

    let withPiece square piece position = { position with board = Board.addPiece square piece position.board}

    let withTurn color position = { position with turn = color }

    let withCastlingRights rights position = { position with castlingRights = rights}

    let withCastlingRight color right position = 
        { position with castlingRights = Castling.withCastlingRight color right position.castlingRights }

    let withEnPassantSquare squareOption position = { position with enPassantSquare = squareOption }

    let withHalfmoveClock clock position = { position with halfmoveClock = clock }

    let withFullMoveNumber number position = { position with fullMoveNumber = number }

    let initialPosition = 
        emptyPosition 
        |> withTurn White
        |> withCastlingRight White Long
        |> withCastlingRight White Short
        |> withCastlingRight Black Long
        |> withCastlingRight Black Short
        |> withEnPassantSquare None
        |> withPiece Square.a1 Piece.whiteRook   |> withPiece Square.a2 Piece.whitePawn
        |> withPiece Square.h1 Piece.whiteRook   |> withPiece Square.h2 Piece.whitePawn
        |> withPiece Square.b1 Piece.whiteKnight |> withPiece Square.b2 Piece.whitePawn
        |> withPiece Square.g1 Piece.whiteKnight |> withPiece Square.g2 Piece.whitePawn
        |> withPiece Square.c1 Piece.whiteBishop |> withPiece Square.c2 Piece.whitePawn
        |> withPiece Square.f1 Piece.whiteBishop |> withPiece Square.f2 Piece.whitePawn
        |> withPiece Square.d1 Piece.whiteQueen  |> withPiece Square.d2 Piece.whitePawn
        |> withPiece Square.e1 Piece.whiteKing   |> withPiece Square.e2 Piece.whitePawn
        |> withPiece Square.a8 Piece.blackRook   |> withPiece Square.a7 Piece.blackPawn
        |> withPiece Square.h8 Piece.blackRook   |> withPiece Square.h7 Piece.blackPawn
        |> withPiece Square.b8 Piece.blackKnight |> withPiece Square.b7 Piece.blackPawn
        |> withPiece Square.g8 Piece.blackKnight |> withPiece Square.g7 Piece.blackPawn
        |> withPiece Square.c8 Piece.blackBishop |> withPiece Square.c7 Piece.blackPawn
        |> withPiece Square.f8 Piece.blackBishop |> withPiece Square.f7 Piece.blackPawn
        |> withPiece Square.d8 Piece.blackQueen  |> withPiece Square.d7 Piece.blackPawn 
        |> withPiece Square.e8 Piece.blackKing   |> withPiece Square.e7 Piece.blackPawn
        |> withFullMoveNumber 1u
        |> withHalfmoveClock 0u

module Move = 
    let create source destination = 
        {
            source = source
            destination = destination
            promotion = None
        }

    let createPromotion source destination promotion = 
        {
            source = source
            destination = destination
            promotion = Some promotion
        }
