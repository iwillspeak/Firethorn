module RedTreeTests

open Xunit

open Firethorn
open Firethorn.Green
open Firethorn.Red

[<Fact>]
let ``Create red tree from root node`` () =
    let green = GreenNode.Create(SyntaxKind 0, [])
    let syntaxRoot = SyntaxNode.CreateRoot(green)

    Assert.Equal(0, syntaxRoot.Offset)
    Assert.Equal(None, syntaxRoot.Parent)

[<Fact>]
let ``Create red tree with children`` () =
    let green =
        GreenNode.Create(
            SyntaxKind 0,
            [ Token(GreenToken.Create(SyntaxKind 1, "("))
              Node(GreenNode.Create(SyntaxKind 4, [ Token(GreenToken.Create(SyntaxKind 3, "*")) ]))
              Node(GreenNode.Create(SyntaxKind 5, [ Token(GreenToken.Create(SyntaxKind 2, "4")) ]))
              Node(GreenNode.Create(SyntaxKind 5, [ Token(GreenToken.Create(SyntaxKind 2, "3")) ]))
              Token(GreenToken.Create(SyntaxKind 1, ")")) ]
        )

    let root = SyntaxNode.CreateRoot(green)

    Assert.Equal(0, root.Offset)
    Assert.Equal(None, root.Parent)

    let literals =
        root.Children()
        |> Seq.filter (fun x -> x.Kind = SyntaxKind 5)

    Assert.Collection(literals, (fun x -> Assert.Equal(2, x.Offset)), (fun x -> Assert.Equal(3, x.Offset)))

    Assert.Empty(
        root.Children()
        |> Seq.filter (fun x -> x.Kind = SyntaxKind 1)
    )

[<Fact>]
let ``Walk red tree`` () =
    let green =
        GreenNode.Create(
            SyntaxKind 0,
            [ Token(GreenToken.Create(SyntaxKind 1, "("))
              Node(GreenNode.Create(SyntaxKind 5, [ Token(GreenToken.Create(SyntaxKind 2, "+")) ]))
              Token(GreenToken.Create(SyntaxKind 1, ")")) ]
        )

    let root = SyntaxNode.CreateRoot(green)

    let events = Walk.walk root

    Assert.Equal(7, events |> Seq.length)

[<Fact>]
let ``Red tree equality of children`` () =

    // Create a simple tree with shared nodes
    let sharedNode =
        GreenNode.Create(SyntaxKind 102, [ GreenToken.Create(SyntaxKind 3, "1") |> Token ])

    let green =
        GreenNode.Create(
            SyntaxKind 101,
            [ Token(GreenToken.Create(SyntaxKind 1, "("))
              Node(sharedNode)
              Node(sharedNode)
              Token(GreenToken.Create(SyntaxKind 2, ")")) ]
        )

    let root = SyntaxNode.CreateRoot(green)

    // Check repeated enumerations are equal
    root.ChildrenWithTokens()
    |> Seq.zip (root.ChildrenWithTokens())
    |> Seq.iter (Assert.Equal)

    let numbers =
        root.Children()
        |> Seq.filter (fun c -> c.Kind = SyntaxKind 102)
        |> List.ofSeq

    Assert.Equal(2, numbers.Length)
    Assert.Equal(numbers.[0].Green, numbers.[1].Green)
    Assert.NotEqual(numbers.[0], numbers.[1])
