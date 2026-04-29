namespace Firethorn.Green

open System.Collections.Concurrent
open Firethorn

/// Cache of green elements. This is used when building trees to share structural sub-trees amongst new nodes.
[<Sealed>]
type GreenCache(maxCachedNodeSize: int) =

    // Maximum number of children that a given node is allowed.
    let size = maxCachedNodeSize

    /// Cache of nodes
    let nodes = ConcurrentDictionary<SyntaxKind * GreenElement list, GreenNode>()

    /// Cache of tokens
    let tokens = ConcurrentDictionary<SyntaxKind * string, GreenToken>()

    /// Get a token for the given `kind` and `value`, returning a cahced one if
    /// available.
    member _.GetToken(kind: SyntaxKind, value: string) =
        tokens.GetOrAdd((kind, value), (GreenToken.Create))

    /// Get a node for the given `kind` and `children`, returning a cached one
    /// if available.
    member _.GetNode(kind: SyntaxKind, children: GreenElement list) =
        if children.Length <= size then
            nodes.GetOrAdd((kind, children), fun (k, cs) -> GreenNode.Create(k, cs))
        else
            GreenNode.Create(kind, children)
