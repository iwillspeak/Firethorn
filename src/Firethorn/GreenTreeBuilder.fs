namespace Firethorn.Green

open Firethorn

/// Builder type for green nodes. This is intended to be used by a
/// parser to build up a tree partwise.
type GreenNodeBuilder() =

    /// Node stack. This contains the cached state each time we
    /// started a node.
    let mutable nodes = []

    /// Current child state. When a node is finished these become the
    /// green child elements for that node.
    let mutable children = []

    /// Start building a new node at the current position of the given
    /// kind.
    member _.StartNode(kind: SyntaxKind) =
        nodes <- (kind, children) :: nodes
        children <- []

    /// Pop a node from the stack and finish it with the current child
    /// state.
    member _.FinishNode() =
        let (kind, oldChildren) = nodes |> List.head
        nodes <- nodes |> List.tail
        let node =
            GreenNode.Create(kind, children |> List.rev)
            |> Node
        children <- node :: oldChildren

    /// Buffer a token into the current node.
    member _.Token(kind: SyntaxKind, text: string) =
        children <- (GreenToken.Create(kind, text) |> Token) :: children

    /// Build a root node of the given `kind` with the current child
    /// state. When this is called the tree must be 'balanced'. That
    /// is no nodes have been begun that haven't been finished.
    ///
    /// This mehthod returns a `GreenNode` directly, rather than a
    /// `GreenElement`. It is inteded that the result of this call be
    /// converted into a red tree by calling `SyntaxNode.CreateRoot`.
    member _.BuildRoot(kind: SyntaxKind) =
        if not (List.isEmpty nodes) then
            failwithf "Expected empty stack. Found %A" nodes 
        
        GreenNode.Create(kind, children |> List.rev)
