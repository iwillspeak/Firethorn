namespace Firethorn.Red

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
type SyntaxNode =
    { Parent: SyntaxNode option
      Offset: int
      Green: GreenNode }

    /// Create a new root syntax node from an underlying green node.
    static member CreateRoot(node: GreenNode) =
        { Parent = None
          Offset = 0
          Green = node }

/// A token within the syntax tree. This is a wrapper around an
/// underlying `GreenToken` in the same way that `SyntaxNode` wraps
/// `GreenNode`.
and SyntaxToken =
    { Parent: SyntaxNode option
      Offset: int
      Green: GreenToken }

/// An element in the 'red' or syntax tree.
and SyntaxElement = NodeOrToken<SyntaxNode, SyntaxToken>
