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