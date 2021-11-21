namespace Firethorn.Green

open Firethorn

/// Syntax Kind
///
/// A generic representation of the kind fo a node in the syntax tree.
[<Struct>]
type SyntaxKind = SyntaxKind of int

/// Green Node
///
/// A single node in the low-level green tree. Nodes in the green tree represent
/// a unique concrete piece of syntax. Green nodes can be shared multiple times
/// witih a tree.
type GreenNode =
    { Kind: SyntaxKind
      Width: int
      Children: GreenElement list }

    /// Create a new green node from raw parts. The width of the node is
    /// inferred from the width of the `children`.
    static member Create(kind: SyntaxKind, children: seq<GreenElement>) =
        let children = List.ofSeq children

        { Kind = kind
          Width =
              children
              |> List.sumBy
                  (function
                  | Node n -> n.TextLength
                  | Token t -> t.TextLength)
          Children = children }

    /// Get the width of the single token.
    member self.TextLength = self.Width

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
    member self.TextLength = self.Text.Length

    /// Custom string representation for tokens.
    override self.ToString() =
        sprintf "@<GreenNode Kind=%A, Text=%s>" self.Kind self.Text

/// A single element in the
and GreenElement = NodeOrToken<GreenNode, GreenToken>
