module GreenTreeTests

open Xunit

open Firethorn
open Firethorn.Green


[<Fact>]
let ``Create token has expected widths`` () =
    let plus = GreenToken.Create(SyntaxKind 1, "+")
    Assert.Equal(1, plus.TextLength)

    let quote =
        GreenToken.Create(SyntaxKind 10, "quote")

    Assert.Equal(5, quote.TextLength)


[<Fact>]
let ``Create token exposes kind`` () =
    let mul = GreenToken.Create(SyntaxKind 0, "*")
    Assert.Equal(SyntaxKind 0, mul.Kind)

    let unquote =
        GreenToken.Create(SyntaxKind 23, "unquote")

    Assert.Equal(SyntaxKind 23, unquote.Kind)

[<Fact>]
let ``Green node has correct length`` () =
    let empty = GreenNode.Create(SyntaxKind 10, [])
    Assert.Equal(0, empty.TextLength)

    let mulForm =
        GreenNode.Create(
            SyntaxKind 1,
            [ GreenToken.Create(SyntaxKind 2, "(") |> Token
              GreenToken.Create(SyntaxKind 3, "*") |> Token
              GreenToken.Create(SyntaxKind 4, "2") |> Token
              GreenToken.Create(SyntaxKind 4, "100") |> Token
              GreenToken.Create(SyntaxKind 5, ")") |> Token ]
        )

    Assert.Equal(7, mulForm.TextLength)

[<Fact>]
let ``Green trees can share nodes`` () =
    let openBrace =
        GreenToken.Create(SyntaxKind 2, "(") |> Token

    let closeBrace =
        GreenToken.Create(SyntaxKind 5, ")") |> Token

    let two =
        GreenToken.Create(SyntaxKind 4, "2") |> Token

    let mulForm =
        GreenNode.Create(
            SyntaxKind 1,
            [ openBrace
              GreenToken.Create(SyntaxKind 3, "*") |> Token
              two
              two
              closeBrace ]
        )

    //               Expression
    //                   |
    //      +---+-----+--+--+----+
    //      |   |      \   /     |
    //   Open  Mul      Two    Close
    Assert.Equal(5, mulForm.TextLength)

    let additionForm =
        GreenNode.Create(
            SyntaxKind 1,
            [ openBrace
              GreenToken.Create(SyntaxKind 6, "+") |> Token
              mulForm |> Node
              mulForm |> Node
              closeBrace ]
        )

    //               Expression
    //                   |
    //      +---+-----+--+--+----+
    //      |   |      \   /     |
    //   Open  Add   Expression  Close
    //      |            |       |
    //      +---+-----+--+--+----+
    //          |      \   /
    //         Mul      Two
    Assert.Equal(13, additionForm.TextLength)
