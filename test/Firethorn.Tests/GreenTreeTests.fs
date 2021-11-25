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

[<Fact>]
let ``Green tokens are structurally equal`` () =
    let helloOne = GreenToken.Create(SyntaxKind 0, "hello")
    let helloTwo = GreenToken.Create(SyntaxKind 0, "hello")
    let helloThree = GreenToken.Create(SyntaxKind 3, "hello")
    let world = GreenToken.Create(SyntaxKind 0, "world")

    Assert.Equal(helloOne, helloTwo)
    Assert.NotEqual(helloOne, world)
    Assert.NotEqual(helloThree, world)
    Assert.NotEqual(helloThree, helloTwo)

[<Fact>]
let ``Green nodes are structurally equal`` () =
    let emptyNode = GreenNode.Create(SyntaxKind 1, [])
    let testToken = GreenToken.Create(SyntaxKind 3, "test")

    let identNode =
        GreenNode.Create(SyntaxKind 2, [ testToken |> Token ])

    Assert.Equal(GreenNode.Create(SyntaxKind 1, []), emptyNode)
    Assert.NotEqual(identNode, emptyNode)
    Assert.Equal(GreenNode.Create(SyntaxKind 2, [ testToken |> Token ]), identNode)

[<Fact>]
let ``Green tree builder`` () =

    let builder = GreenNodeBuilder()

    builder.Token(SyntaxKind 1, "(")
    builder.StartNode(SyntaxKind 101)
    builder.Token(SyntaxKind 2, "*")
    builder.FinishNode()
    builder.StartNode(SyntaxKind 102)
    builder.Token(SyntaxKind 3, "10")
    builder.FinishNode()
    builder.StartNode(SyntaxKind 102)
    builder.Token(SyntaxKind 3, "10")
    builder.FinishNode()
    builder.Token(SyntaxKind 4, ")")

    let tree = builder.BuildRoot(SyntaxKind 103)

    Assert.Equal((SyntaxKind 103), tree.Kind)

    Assert.Collection(
        tree.Children,
        (fun token -> Assert.True(token |> NodeOrToken.isToken)),
        (fun node ->
            Assert.True(node |> NodeOrToken.isNode)
            let node = (node |> NodeOrToken.asNode).Value
            Assert.Equal(SyntaxKind 101, node.Kind)),
        (fun node ->
            Assert.True(node |> NodeOrToken.isNode)
            let node = (node |> NodeOrToken.asNode).Value
            Assert.Equal(SyntaxKind 102, node.Kind)),
        (fun node ->
            Assert.True(node |> NodeOrToken.isNode)
            let node = (node |> NodeOrToken.asNode).Value
            Assert.Equal(SyntaxKind 102, node.Kind)),
        (fun token -> Assert.True(token |> NodeOrToken.isToken))
    )
