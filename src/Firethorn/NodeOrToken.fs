namespace Firethorn

/// Syntax Kind
///
/// A generic representation of the kind fo a node in the syntax tree.
[<Struct>]
type SyntaxKind = SyntaxKind of int

/// Node or Token Type
///
/// Both the green and red tees depend on storing references to either a node
/// or a token. This type is used as a generic wrapper around a node type
/// and token type.
[<Struct>]
type NodeOrToken<'N, 'T> =
    | Node of node: 'N
    | Token of token: 'T

module NodeOrToken =

    /// Check if the given `NodeOrToken` is a `Node`.
    let isNode =
        function
        | Node _ -> true
        | Token _ -> false

    /// Check if the given `NodeOrToken` is a `Token`.
    let isToken =
        function
        | Node _ -> false
        | Token _ -> true

    /// Map the parts of a `NodeOrToken`.
    let map onNode onToken =
        function
        | Node n -> n |> onNode |> Node
        | Token t -> t |> onToken |> Token

    /// Unify a `NodeOrToken` into a new value.
    let consolidate onNode onToken =
        function
        | Node n -> n |> onNode
        | Token t -> t |> onToken

    /// Convert a node or token into an optional node.
    let asNode =
        function
        | Node n -> Some n
        | Token _ -> None

    /// Convert a node or token into an optional token.
    let asToken =
        function
        | Node _ -> None
        | Token t -> Some t
