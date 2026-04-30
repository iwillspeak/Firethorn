module GreenCacheTets

open Firethorn
open Firethorn.Green
open Xunit

[<Fact>]
let ``Green cache returns cached tokens`` () =
    let cache = GreenCache(3)

    let first = cache.GetToken(SyntaxKind 102, "hello")
    let second = cache.GetToken(SyntaxKind 102, "hello")

    Assert.Equal(first, second)
    Assert.Same(first, second)

[<Fact>]
let ``Green cache returns distinct tokens for distinct keys`` () =
    let cache = GreenCache(3)

    let byKind102 = cache.GetToken(SyntaxKind 102, "hello")
    let byKind101 = cache.GetToken(SyntaxKind 101, "hello")

    Assert.NotSame(byKind102, byKind101)
    Assert.NotEqual(byKind102, byKind101)

[<Fact>]
let ``Green cache returns cached nodes`` () =
    let helloToken = GreenToken.Create(SyntaxKind 101, "hello")
    let emptyNode = GreenNode.Create(SyntaxKind 102, [])
    let cache = GreenCache(3)

    let first =
        cache.GetNode(SyntaxKind 101, [| helloToken |> Token; emptyNode |> Node |])

    let second =
        cache.GetNode(SyntaxKind 101, [| helloToken |> Token; emptyNode |> Node |])

    Assert.Same(first, second)

[<Fact>]
let ``Green cache returns distinct nodes for distinct keys`` () =
    let helloToken = GreenToken.Create(SyntaxKind 101, "hello")
    let emptyNode = GreenNode.Create(SyntaxKind 102, [])
    let cache = GreenCache(3)

    let withChildren =
        cache.GetNode(SyntaxKind 101, [| helloToken |> Token; emptyNode |> Node |])

    let empty = cache.GetNode(SyntaxKind 102, [||])

    Assert.NotSame(withChildren, empty)
