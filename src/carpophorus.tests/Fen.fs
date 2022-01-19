module Tests

open Expecto
open Fen
open Model

[<Tests>]
let tests =
  testList "FEN" [
    test "loadPosition" {
        let fen = ""
        let position = loadPosition fen
        let expected = Position.emptyPosition
    }
  ]
