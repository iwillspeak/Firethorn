namespace Firethorn.Green

open System
open Firethorn

/// Cache of green elements. This is used when building trees to share structural sub-trees amongst new nodes.
/// Implemented as a pair of direct-mapped arrays (one for keys, one for values) with 4096 slots each.
/// On collision the existing entry is evicted and replaced.
[<Sealed>]
type GreenCache(maxCachedNodeSize: int) =

    let size = maxCachedNodeSize

    let nodeKeys = Array.zeroCreate<struct(SyntaxKind * GreenElement[]) voption> 4096
    let nodeValues = Array.zeroCreate<GreenNode> 4096

    let tokenKeys = Array.zeroCreate<struct(SyntaxKind * string) voption> 4096
    let tokenValues = Array.zeroCreate<GreenToken> 4096

    static let nodeHash (kind: SyntaxKind) (children: GreenElement[]) =
        children
        |> Array.fold (fun acc el -> HashCode.Combine(acc, el.GetHashCode())) (kind.GetHashCode())

    member _.GetToken(kind: SyntaxKind, value: string) =
        let hash = HashCode.Combine(kind.GetHashCode(), value.GetHashCode())
        let idx = hash &&& 0xFFF
        match tokenKeys.[idx] with
        | ValueSome(struct(k, v)) when k = kind && v = value ->
            tokenValues.[idx]
        | _ ->
            let token = GreenToken.Create(kind, value)
            tokenKeys.[idx] <- ValueSome(struct(kind, value))
            tokenValues.[idx] <- token
            token

    member _.GetNode(kind: SyntaxKind, children: GreenElement[]) =
        if children.Length > size then
            GreenNode.Create(kind, children)
        else
            let hash = nodeHash kind children
            let idx = hash &&& 0xFFF
            match nodeKeys.[idx] with
            | ValueSome(struct(k, cs)) when
                k = kind
                && cs.Length = children.Length
                && Array.forall2 (=) cs children ->
                nodeValues.[idx]
            | _ ->
                let node = GreenNode.Create(kind, children)
                nodeKeys.[idx] <- ValueSome(struct(kind, children))
                nodeValues.[idx] <- node
                node
