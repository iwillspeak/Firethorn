# Firethorn

Red-Green syntax trees for F#. Inspired by Rowan.

## Design Goals

 * Compact representation of trees.
 * Complete representation of the input text.
 * Trees should be cheaply updateable.
 * Handle partial data and errors.
 * Query nodes in the tree by their location in the source.

## Architecture

There are two main layers to the syntax trees supported by
Firethorn. The `Green` tree represents abstract syntactic data; the
`Red` or `Syntax` tree attaches location and parent information to
green tree nodes.

Trees which share structure can share portions of their green
tree. e.g. in `1 + 1` the node for `1` may be the same green node. The
red nodes however would differ as the locations within the tree are
different.

A simplified definition of the green tree contains the following:

```f#
type GreenToken =
	{ Kind: SyntaxKind
	  Text: string }
and GreenNode =
	{ Kind: SyntaxKind
	  Width: int
	  Children: Choice<GreenNode, GreenToken> list }
```

In the green tree each node has no specific location, only a
width. The width of a token is the width of the text it contains. The
with of nodes is cached on the node. It is the sum of the widths of
its children.

The red tree is the a layer over this tree similar to the following:

```f#
type SyntaxToken =
	{ Offset: int
	  Parent: SyntaxNode option
	  Green: GreenToken }
and SyntaxNode =
	{ Offset: int
	  Parent: SyntaxNode option
	  Green: GreenNode }
```

This adds absolute offsets of the start of each node. This means a
green node on its own has no specific location. It only has a location
derived from the offset of its parent red node.

## AST Layer

On top of this low-level untyped tree we can then layer a typed
AST. This is done by having `cast` methods for each node type that
take the underlying `SyntaxNode` and provide a typed wrapper. An
example for a simplified binary expression node:

```F#
type OperatorSyntax(syntax: SyntaxToken) =
	
	static member Cast(node: SyntaxNode) =
		if node.Kind = SyntaxKind PLUS then
			Some(OperatorSyntax(node))
		else
			None

type ConstantSyntax(syntax: SyntaxNode) =

	static member Cast(node: SyntaxNode) =
		if node.Kind = SyntaxKind CONST then
			Some(ConstantSyntax(node))
		else
			None

and BinarySyntax(syntax: SyntaxNode) =
	
	static member Cast(node: SyntaxNode) =
		if node.Kind = SyntaxKind BINEXPR then
			Some(BinarySyntax(node))
		else
			None

	member _.Left =
		syntax.Children()
		|> Seq.choose ExpressionSyntax.Cast
		|> Seq.tryHead

	member _.Operator =
		syntax.ChildrenWithTokens()
		|> Seq.tryPick(
			NodeOrToken.asToken
			>> (Option.bind OperatorSyntax.Cast)
		)
	
	member _.Right =
		syntax.Children()
		|> Seq.choose ExpressionSyntax.Cast
		|> Seq.skip 1
		|> Seq.tryHead

and ExpressionSyntax =
	| Bin of BinarySyntax
	| Const of ConstantSyntax
	
	static member Cast(node: SyntaxNode) =
		(BinarySyntax.Cast node |> Option.map Bin)
		|> Option.orElseWith (fun () -> ConstantSyntax.Cast |> Option.map Const)
		
```

In this model we have strongly typed tree structure defined by a union
type `ExpressionSyntax`. Each node type in the tree can expose the
important parts of that node with strongly typed properties. All
properties return either `Option` or `Seq` values to model the fact
that any part of the tree could be missing or empty.

## Building Parse Trees

Trees can be built from the bottom up by repeatedly calling `GreenToken.Create` and `GreenNode.Create`. The final node can then be converted into a red tree with the `SyntaxNode.CreateRoot` method. This allows for ergonomic building of syntax during testing, and provides a simple API for programatically generated syntax such as macro expansion. For some simple languages and parsers this may be enough.

A higher level API is also provided to build nodes using `GreenNodeBuilder`. This type allows for incremental building of nodes and is particularly suited to top-down parser construction:

```f#
let builder = GreenNodeBuilder()
builder.StartNode(SytaxKind BINOP)
builder.Token(SyntaxKind OP, "+")
builder.Token(SyntaxKind NUM, "12")
builder.Token(SyntaxKind NUM, "45")
builder.FinishNode()
```

For cases where it isn't known ahead of time if a node will be needed the `Mark` API allows a point to be 'marked' and applied later to create a node. This would be equivalent to the above:

```f#
let mark = builder.Mark()
builder.Token(SyntaxKind OP, "+")
builder.Token(SyntaxKind NUM, "12")
builder.Token(SyntaxKind NUM, "45")
builder.ApplyMark(mark, SytaxKind BINOP)
```

Once a tee is built it can be converted into a single root node with the `BuildRoot` API:

```f#
let syntaxRoot = builder.BuildRoot(SyntaxKind PROGRAM)
```