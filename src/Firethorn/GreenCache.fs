namespace Firethorn.Green

open Firethorn
open System.Collections.Concurrent

/// Cache of green elements. This is used when building trees to share structural sub-trees amongst new nodes.
[<Sealed>]
type GreenCache(maxCachedNodeSize: int) =

    // Maximum number of children that a given node is allowed.
    let size = maxCachedNodeSize

    /// Cache of nodes
    let nodes =
        ConcurrentDictionary<SyntaxKind * GreenElement list, GreenNode>()

    /// Cache of tokens
    let tokens =
        ConcurrentDictionary<SyntaxKind * string, GreenToken>()

    /// Get a token for the given `kind` and `value`, returning a cahced one if
    /// available.
    member _.GetToken(kind: SyntaxKind, value: string) =
        tokens.GetOrAdd((kind, value), (GreenToken.Create))

    /// Get a node for the given `kind` and `children`, returning a cached one
    /// if available.
    member _.GetNode(kind: SyntaxKind, children: GreenElement list) =
        if children.Length <= size then
            // TODO: Limit the size of cached nodes here. Over a certain size it's
            //       better to just fabricate a new node each time rather than doing
            //       a cache lookup.
            nodes.GetOrAdd((kind, children), (GreenNode.Create))
        else
            GreenNode.Create(kind, children)
