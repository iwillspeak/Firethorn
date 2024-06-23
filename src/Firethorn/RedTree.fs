namespace Firethorn.Red

open System
open System.Runtime.CompilerServices

open Firethorn
open Firethorn.Green

/// A node in the syntax tree. This is represented by a green node
/// holding the underlying syntax structure; and an offset of this
/// node within the source file.
///
/// While a `GreenNode` represents abstract syntactic element a
/// `SyntaxNode` represents a specific piece of syntax within the
/// tree. Two `SyntaxNodes` are equal iff they represent the same
/// element in the source text.
[<NoComparison; CustomEquality>]
type SyntaxNode =
    { Parent: SyntaxNode option
      Offset: TextLength
      Green: GreenNode }

    /// Create a new root syntax node from an underlying green node.
    static member CreateRoot(node: GreenNode) =
        { Parent = None
          Offset = 0
          Green = node }

    /// Get the kind of the underlying green node.
    member self.Kind = self.Green.Kind

    /// The text range covered by this element.
    member self.Range =
        { Start = self.Offset
          End = self.Offset + self.Green.TextLength }

    /// Enumerate the children of the current node.
    member self.ChildrenWithTokens() =
        let (children, _) =
            self.Green.Children
            |> Seq.mapFold
                (fun idx green ->
                    match green with
                    | Node greenNode ->
                        ({ SyntaxNode.Parent = Some(self)
                           Offset = idx
                           Green = greenNode }
                         |> Node,
                         idx + greenNode.TextLength)
                    | Token greenToken ->
                        ({ SyntaxToken.Parent = Some(self)
                           Offset = idx
                           Green = greenToken }
                         |> Token,
                         idx + greenToken.TextLength))
                self.Offset

        children

    /// Enumerate the child nodes only of the current node, skipping tokens.
    member self.Children() =
        self.ChildrenWithTokens() |> Seq.choose (NodeOrToken.asNode)

    /// Custom equality. Red nodes are considered equal if they wrap the exact
    /// same green node and have the same parent. There's no need to recursively
    /// check the green node for equality here.
    override self.Equals(other: obj) =
        match other with
        | :? SyntaxNode as other ->
            self.Parent = other.Parent
            && self.Offset = other.Offset
            && obj.ReferenceEquals(self.Green, other.Green)
        | _ -> false

    /// Custom hash code to mimic the equality behavior.
    override self.GetHashCode() =
        HashCode.Combine(self.Parent, RuntimeHelpers.GetHashCode(self.Green))

/// A token within the syntax tree. This is a wrapper around an
/// underlying `GreenToken` in the same way that `SyntaxNode` wraps
/// `GreenNode`.
and [<NoComparison; CustomEquality>] SyntaxToken =
    { Parent: SyntaxNode option
      Offset: TextLength
      Green: GreenToken }

    /// Get the kind of the underlying green node.
    member self.Kind = self.Green.Kind

    /// The text range covered by this element.
    member self.Range =
        { Start = self.Offset
          End = self.Offset + self.Green.TextLength }

    /// Custom equality. Red tokens are considered equal if they wrap the exact
    /// same green token, and have the same parent. There's no need to
    /// recursively check the green node for equality here.
    override self.Equals(other: obj) =
        match other with
        | :? SyntaxToken as other ->
            self.Parent = other.Parent
            && self.Offset = other.Offset
            && obj.ReferenceEquals(self.Green, other.Green)
        | _ -> false

    /// Custom hash code to mimic the equality behavior.
    override self.GetHashCode() =
        HashCode.Combine(self.Parent, RuntimeHelpers.GetHashCode(self.Green))


/// An element in the 'red' or syntax tree.
and SyntaxElement = NodeOrToken<SyntaxNode, SyntaxToken>
