namespace Firethorn.Green

open Firethorn

[<Struct>]
type Mark =
    private
        { Ref: ResizeArray<GreenElement>
          Count: int }

/// Builder type for green nodes. This is intended to be used by a
/// parser to build up a tree partwise.
type GreenNodeBuilder(cache: GreenCache) =

    /// Node stack. Each entry records the kind and the parent's children
    /// buffer; the current level's children live in `children`.
    let mutable nodes: (SyntaxKind * ResizeArray<GreenElement>) list = []

    /// Children accumulated for the current nesting level, in forward order.
    let mutable children = ResizeArray<GreenElement>()

    /// Cache for nodes and tokens
    let nodeCache = cache

    /// Create a `GreenNodeBuilder` with a new node cache.
    new() = GreenNodeBuilder(GreenCache(3))

    /// Start building a new node at the current position of the given kind.
    member _.StartNode(kind: SyntaxKind) =
        nodes <- (kind, children) :: nodes
        children <- ResizeArray()

    /// Pop a node from the stack and finish it with the current child state.
    member _.FinishNode() =
        if List.isEmpty nodes then
            invalidOp "Unbalanced call to `FinishNode`."

        let (kind, parent) = List.head nodes
        nodes <- List.tail nodes

        let node = nodeCache.GetNode(kind, children.ToArray()) |> Node
        children <- parent
        children.Add(node)

    /// Store a mark to the current state. This can optionally be used later
    /// to convert the buffered state into a node as if `StartNode` was called
    /// at this point.
    member _.Mark() =
        { Ref = children
          Count = children.Count }

    /// Convert a stored mark into a node. This takes all state buffered since
    /// the mark and uses it as the child state of the new node. The final state
    /// of the builder is that at the time the mark was taken, with a new node
    /// of `kind` added.
    member _.ApplyMark(mark: Mark, kind: SyntaxKind) =
        if not (obj.ReferenceEquals(mark.Ref, children)) then
            invalidOp "Mark has expired. State has unwound past mark."

        if children.Count < mark.Count then
            invalidOp "Mark has expired. State has unwound past mark."

        let count = children.Count - mark.Count
        let arr = Array.zeroCreate count
        children.CopyTo(mark.Count, arr, 0, count)
        children.RemoveRange(mark.Count, count)

        let node = nodeCache.GetNode(kind, arr) |> Node
        children.Add(node)

    /// Buffer a token into the current node.
    member _.Token(kind: SyntaxKind, text: string) =
        children.Add(nodeCache.GetToken(kind, text) |> Token)

    /// Build a root node of the given `kind` with the current child state.
    /// When this is called the tree must be 'balanced': no nodes have been
    /// begun that haven't been finished.
    ///
    /// This method returns a `GreenNode` directly, rather than a
    /// `GreenElement`. It is intended that the result of this call be
    /// converted into a red tree by calling `SyntaxNode.CreateRoot`.
    member _.BuildRoot(kind: SyntaxKind) =
        if not (List.isEmpty nodes) then
            sprintf "Expected empty stack. Found %A" nodes |> invalidOp

        nodeCache.GetNode(kind, children.ToArray())
