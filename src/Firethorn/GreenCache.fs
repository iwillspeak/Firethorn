namespace Firethorn.Green

open System
open Firethorn

[<Struct>]
type private NodeSlot =
    { mutable Key: struct(SyntaxKind * GreenElement[]) voption
      mutable Value: GreenNode }

[<Struct>]
type private TokenSlot =
    { mutable Key: struct(SyntaxKind * string) voption
      mutable Value: GreenToken }

/// Cache of green elements. This is used when building trees to share structural sub-trees amongst new nodes.
/// Implemented as two direct-mapped arrays (nodes and tokens) of 512 slots each. On collision the existing
/// entry is evicted and replaced.
[<Sealed>]
type GreenCache(maxCachedNodeSize: int) =

    let size = maxCachedNodeSize
    let nodes = Array.zeroCreate<NodeSlot> 512
    let tokens = Array.zeroCreate<TokenSlot> 512

    static let nodeHash (kind: SyntaxKind) (children: GreenElement[]) =
        children
        |> Array.fold (fun acc el -> HashCode.Combine(acc, el.GetHashCode())) (kind.GetHashCode())

    member _.GetToken(kind: SyntaxKind, value: string) =
        let hash = HashCode.Combine(kind.GetHashCode(), value.GetHashCode())
        let idx = hash &&& 0x1FF
        let slot = tokens.[idx]
        match slot.Key with
        | ValueSome(struct(k, v)) when k = kind && v = value ->
            slot.Value
        | _ ->
            let token = GreenToken.Create(kind, value)
            tokens.[idx] <- { Key = ValueSome(struct(kind, value)); Value = token }
            token

    member _.GetNode(kind: SyntaxKind, children: GreenElement[]) =
        if children.Length > size then
            GreenNode.Create(kind, children)
        else
            let hash = nodeHash kind children
            let idx = hash &&& 0x1FF
            let slot = nodes.[idx]
            match slot.Key with
            | ValueSome(struct(k, cs)) when
                k = kind
                && cs.Length = children.Length
                && Array.forall2 (=) cs children ->
                slot.Value
            | _ ->
                let node = GreenNode.Create(kind, children)
                nodes.[idx] <- { Key = ValueSome(struct(kind, children)); Value = node }
                node
