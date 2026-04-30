namespace Firethorn.Green

open System
open System.Collections.Concurrent
open System.Collections.Generic
open Firethorn

/// Structural equality comparer for `(SyntaxKind * GreenElement[])` cache keys.
/// Arrays do not have structural equality by default, so this comparer provides
/// element-wise comparison using the structural equality of `GreenElement`.
type private GreenNodeKeyComparer() =
    interface IEqualityComparer<SyntaxKind * GreenElement[]> with
        member _.Equals((k1, c1), (k2, c2)) =
            k1 = k2 && c1.Length = c2.Length && Array.forall2 (=) c1 c2

        member _.GetHashCode((k, c)) =
            c
            |> Array.fold (fun acc el -> HashCode.Combine(acc, el.GetHashCode())) (k.GetHashCode())

/// Cache of green elements. This is used when building trees to share structural sub-trees amongst new nodes.
[<Sealed>]
type GreenCache(maxCachedNodeSize: int) =

    // Maximum number of children that a given node is allowed.
    let size = maxCachedNodeSize

    /// Cache of nodes
    let nodes =
        ConcurrentDictionary<SyntaxKind * GreenElement[], GreenNode>(GreenNodeKeyComparer())

    /// Cache of tokens
    let tokens = ConcurrentDictionary<SyntaxKind * string, GreenToken>()

    /// Get a token for the given `kind` and `value`, returning a cahced one if
    /// available.
    member _.GetToken(kind: SyntaxKind, value: string) =
        tokens.GetOrAdd((kind, value), (GreenToken.Create))

    /// Get a node for the given `kind` and `children`, returning a cached one
    /// if available.
    member _.GetNode(kind: SyntaxKind, children: GreenElement[]) =
        if children.Length <= size then
            nodes.GetOrAdd((kind, children), fun (k, cs) -> GreenNode.Create(k, cs))
        else
            GreenNode.Create(kind, children)
