module GreenTreeTests

open Xunit

open Firethorn
open Firethorn.Green
open System


[<Fact>]
let ``Create token has expected widths`` () =
    let plus = GreenToken.Create(SyntaxKind 1, "+")
    Assert.Equal(1u, plus.TextLength)

    let quote = GreenToken.Create(SyntaxKind 10, "quote")

    Assert.Equal(5u, quote.TextLength)


[<Fact>]
let ``Create token exposes kind`` () =
    let mul = GreenToken.Create(SyntaxKind 0, "*")
    Assert.Equal(SyntaxKind 0, mul.Kind)

    let unquote = GreenToken.Create(SyntaxKind 23, "unquote")

    Assert.Equal(SyntaxKind 23, unquote.Kind)

[<Fact>]
let ``Green node has correct length`` () =
    let empty = GreenNode.Create(SyntaxKind 10, [])
    Assert.Equal(0u, empty.TextLength)

    let mulForm =
        GreenNode.Create(
            SyntaxKind 1,
            [ GreenToken.Create(SyntaxKind 2, "(") |> Token
              GreenToken.Create(SyntaxKind 3, "*") |> Token
              GreenToken.Create(SyntaxKind 4, "2") |> Token
              GreenToken.Create(SyntaxKind 4, "100") |> Token
              GreenToken.Create(SyntaxKind 5, ")") |> Token ]
        )

    Assert.Equal(7u, mulForm.TextLength)

[<Fact>]
let ``Green trees can share nodes`` () =
    let openBrace = GreenToken.Create(SyntaxKind 2, "(") |> Token

    let closeBrace = GreenToken.Create(SyntaxKind 5, ")") |> Token

    let two = GreenToken.Create(SyntaxKind 4, "2") |> Token

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
    Assert.Equal(5u, mulForm.TextLength)

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
    Assert.Equal(13u, additionForm.TextLength)

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

    let identNode = GreenNode.Create(SyntaxKind 2, [ testToken |> Token ])

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
        Action<GreenElement>(fun token -> Assert.True(token |> NodeOrToken.isToken)),
        Action<GreenElement>(fun node ->
            Assert.True(node |> NodeOrToken.isNode)
            let node = (node |> NodeOrToken.asNode).Value
            Assert.Equal(SyntaxKind 101, node.Kind)),
        Action<GreenElement>(fun node ->
            Assert.True(node |> NodeOrToken.isNode)
            let node = (node |> NodeOrToken.asNode).Value
            Assert.Equal(SyntaxKind 102, node.Kind)),
        Action<GreenElement>(fun node ->
            Assert.True(node |> NodeOrToken.isNode)
            let node = (node |> NodeOrToken.asNode).Value
            Assert.Equal(SyntaxKind 102, node.Kind)),
        Action<GreenElement>(fun token -> Assert.True(token |> NodeOrToken.isToken))
    )

[<Fact>]
let ``Complete builder with unbalanced node throws exception`` () =
    let builder = GreenNodeBuilder()
    builder.StartNode(SyntaxKind 1)
    Assert.Throws<InvalidOperationException>(fun () -> builder.BuildRoot(SyntaxKind 2) |> ignore)

[<Fact>]
let ``Finish of unstarted node throws exception`` () =
    let builder = GreenNodeBuilder()

    let exn = Assert.Throws<InvalidOperationException>(fun () -> builder.FinishNode())

    Assert.Contains("Unbalanced call to `FinishNode`.", exn.Message)

    builder.StartNode(SyntaxKind 1)
    builder.FinishNode()

    let exn = Assert.Throws<InvalidOperationException>(fun () -> builder.FinishNode())

    Assert.Contains("Unbalanced call to `FinishNode`.", exn.Message)

[<Fact>]
let ``Mark and ApplyMark wraps tokens added since mark`` () =
    let builder = GreenNodeBuilder()

    builder.Token(SyntaxKind 1, "(")
    let mark = builder.Mark()
    builder.Token(SyntaxKind 2, "x")
    builder.Token(SyntaxKind 2, "y")
    builder.ApplyMark(mark, SyntaxKind 100)
    builder.Token(SyntaxKind 1, ")")

    let tree = builder.BuildRoot(SyntaxKind 200)

    Assert.Equal(SyntaxKind 200, tree.Kind)
    Assert.Collection(
        tree.Children,
        Action<GreenElement>(fun e -> Assert.True(e |> NodeOrToken.isToken)),
        Action<GreenElement>(fun e ->
            let n = (e |> NodeOrToken.asNode).Value
            Assert.Equal(SyntaxKind 100, n.Kind)
            Assert.Collection(
                n.Children,
                Action<GreenElement>(fun t -> Assert.True(t |> NodeOrToken.isToken)),
                Action<GreenElement>(fun t -> Assert.True(t |> NodeOrToken.isToken)))),
        Action<GreenElement>(fun e -> Assert.True(e |> NodeOrToken.isToken))
    )

[<Fact>]
let ``Mark and ApplyMark wraps sub-nodes built since mark`` () =
    let builder = GreenNodeBuilder()

    let mark = builder.Mark()
    builder.StartNode(SyntaxKind 10)
    builder.Token(SyntaxKind 1, "a")
    builder.FinishNode()
    builder.StartNode(SyntaxKind 11)
    builder.Token(SyntaxKind 2, "b")
    builder.FinishNode()
    builder.ApplyMark(mark, SyntaxKind 100)

    let tree = builder.BuildRoot(SyntaxKind 200)

    Assert.Collection(
        tree.Children,
        Action<GreenElement>(fun e ->
            let n = (e |> NodeOrToken.asNode).Value
            Assert.Equal(SyntaxKind 100, n.Kind)
            Assert.Equal(2, n.Children.Length))
    )

[<Fact>]
let ``ApplyMark with expired mark throws`` () =
    // Take a mark inside a node, then finish that node — the stack has
    // unwound past the mark's level. Even if new tokens are added until
    // the child count matches, the mark must be rejected.
    let builder = GreenNodeBuilder()

    builder.StartNode(SyntaxKind 10)
    builder.Token(SyntaxKind 1, "a")
    let mark = builder.Mark()
    builder.Token(SyntaxKind 2, "b")
    builder.FinishNode()
    // One token at the outer level to restore the same child count as at mark time
    builder.Token(SyntaxKind 3, "c")

    let exn =
        Assert.Throws<InvalidOperationException>(fun () ->
            builder.ApplyMark(mark, SyntaxKind 100) |> ignore)

    Assert.Contains("Mark has expired", exn.Message)
