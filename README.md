# Firethorn

Red-Green syntax trees for F#. Inspired by Rowan.

## Requirements

 * Compact representation of trees.
 * Complete representation of the input text.
 * Trees should be cheaply updateable.
 * Handle partial data and errors.
 * Query nodes in the tree by their location in the source.

## Architecture

There are two main layers to the syntax trees supported by
Firethorn. The `Green` tree represents abstract syntactic data; the `Red` or `Syntax` tree attaches location and parent information to green tree nodes.

Trees which share structure can share portions of their green tree. e.g. in `1 + 1` the node for `1` may be the same green node. The red nodes however would differ as the locations within the tree are different.

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

In the green tree each node has no specific location, only a width. The width of a token is the width of the text it contains. The with of nodes is cached on the node. It is the sum of the widths of its children.

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

This adds absolute offsets of the start of each node. This means a green node on its own has no specific location. It only has a location derived from the offset of its parent red node.
