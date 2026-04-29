namespace Firethorn.Green

open System
open Firethorn


/// Green Node
///
/// A single node in the low-level green tree. Nodes in the green tree represent
/// a unique concrete piece of syntax. Green nodes can be shared multiple times
/// witih a tree.
[<CustomEquality; NoComparison>]
type GreenNode =
    { Kind: SyntaxKind
      Width: TextLength
      Children: GreenElement[]
      Hash: int }

    /// Create a new green node from raw parts. The width of the node is
    /// inferred from the width of the `children`.
    static member Create(kind: SyntaxKind, children: seq<GreenElement>) =
        let children = Array.ofSeq children

        let hash =
            children
            |> Array.fold (fun acc el -> HashCode.Combine(acc, el.GetHashCode())) (kind.GetHashCode())

        { Kind = kind
          Width =
            children
            |> Array.sumBy (function
                | Node n -> n.TextLength
                | Token t -> t.TextLength)
          Children = children
          Hash = hash }

    /// Get the width of the single token.
    member self.TextLength = self.Width

    /// Two green nodes are equal when they have the same kind and identical
    /// children. The pre-computed `Hash` is used as a fast-rejection guard.
    override self.Equals(other: obj) =
        match other with
        | :? GreenNode as other ->
            obj.ReferenceEquals(self, other)
            || (self.Hash = other.Hash
                && self.Kind = other.Kind
                && self.Children.Length = other.Children.Length
                && Array.forall2 (fun a b -> a = b) self.Children other.Children)
        | _ -> false

    /// Returns the pre-computed structural hash.
    override self.GetHashCode() = self.Hash

/// Green Token
///
/// A terminal token in the tree. This contains the raw lexeme value that was
/// matched.
and GreenToken =
    { Kind: SyntaxKind
      Text: string }

    /// Create a new green token from the raw parts.
    static member Create(kind: SyntaxKind, text: string) = { Kind = kind; Text = text }

    /// Get the width of the single token.
    member self.TextLength = uint32 self.Text.Length

    /// Custom string representation for tokens.
    override self.ToString() =
        sprintf "@<GreenNode Kind=%A, Text=%s>" self.Kind self.Text

/// A single element in the
and GreenElement = NodeOrToken<GreenNode, GreenToken>
