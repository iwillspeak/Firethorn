module GreenCacheTets

open Firethorn
open Firethorn.Green
open Xunit

[<Fact>]
let ``Green cache returns cached tokens`` () =
    let cache = GreenCache(3)

    let firstIdent = cache.GetToken(SyntaxKind 102, "hello")
    let secondIdent = cache.GetToken(SyntaxKind 101, "hello")
    let thirdIdent = cache.GetToken(SyntaxKind 102, "hello")

    Assert.Equal(firstIdent, thirdIdent)
    Assert.Same(firstIdent, thirdIdent)
    Assert.NotSame(firstIdent, secondIdent)
    Assert.NotEqual(firstIdent, secondIdent)

[<Fact>]
let ``Green cache returns cached nodes`` () =
    let helloToken = GreenToken.Create(SyntaxKind 101, "hello")

    let emptyNode = GreenNode.Create(SyntaxKind 102, [])
    let cache = GreenCache(3)

    let first =
        cache.GetNode(
            SyntaxKind 101,
            [ helloToken |> Token
              emptyNode |> Node ]
        )

    let second = cache.GetNode(SyntaxKind 102, [])

    let third =
        cache.GetNode(
            SyntaxKind 101,
            [ helloToken |> Token
              emptyNode |> Node ]
        )

    Assert.NotSame(first, second)
    Assert.Same(first, third)
