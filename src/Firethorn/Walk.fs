namespace Firethorn.Red

open Firethorn

/// Events emitted by the walk
type WalkEvent =
    /// A new syntax node is entered.
    | EnterNode of SyntaxNode
    /// A syntax node is left
    | LeaveNode of SyntaxNode
    /// A syntax token is encountered.
    | OnToken of SyntaxToken

module Walk =

    /// Walk the descendants of the given node.
    let rec public walk node =
        seq {
            yield EnterNode(node)

            for child in node.ChildrenWithTokens() do
                match child with
                | Node n -> yield! walk n
                | Token t -> yield OnToken(t)

            yield LeaveNode(node)
        }
