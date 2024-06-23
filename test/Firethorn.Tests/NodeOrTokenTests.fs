module Tests

open Xunit

open Firethorn

[<Fact>]
let ``Check isNode`` () =
    Assert.True(Node 10 |> NodeOrToken.isNode)
    Assert.False(Token 10 |> NodeOrToken.isNode)

[<Fact>]
let ``Check isToken`` () =
    Assert.False(Node 10 |> NodeOrToken.isToken)
    Assert.True(Token 10 |> NodeOrToken.isToken)

[<Fact>]
let ``Map maps node values`` () =
    Assert.Equal(Node 100, Node 10 |> NodeOrToken.map (fun x -> 10 * x) (failwithf "Unexpected %A"))

    Assert.Equal(Token 100, Token 10 |> NodeOrToken.map (failwithf "Unexpected %A") (fun x -> 10 * x))
