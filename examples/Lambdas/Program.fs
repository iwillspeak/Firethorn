open Firethorn.Red
open Microsoft.FSharp.Core.Printf
open System.Text

module Parse =

    open System

    open Firethorn
    open Firethorn.Green

    type TokenKind =
        | Lambda
        | Dot
        | Ident
        | Whitespace
        | Newline
        | EndOfFile
        | Error

    type AstKind =
        | ERROR = -1

        // Token kinds
        | IDENT = 1
        | LAMBDA = 2
        | DOT = 3
        | END = 4
        | WHITESPACE = 5

        /// Node kinds
        | USE = 100
        | ABSTRACTION = 101
        | APPLICATION = 102
        | PROGRAM = 103

    let astToGreen (kind: AstKind) = SyntaxKind(int kind)

    let astFromGreen (kind: SyntaxKind) =
        match kind with
        | SyntaxKind d -> enum<AstKind> d

    let lex arg =
        seq {
            for c in arg do
                let kind =
                    match c with
                    | '\\' -> TokenKind.Lambda
                    | '.' -> TokenKind.Dot
                    | '\n' -> TokenKind.Newline
                    | c when Char.IsLetter(c) -> TokenKind.Ident
                    | c when Char.IsWhiteSpace(c) -> TokenKind.Whitespace
                    | _ -> TokenKind.Error

                yield (kind, c.ToString())
        }

    let parse arg =

        let mutable tokens = lex arg |> List.ofSeq

        let getText tok =
            let (_, text) = tok
            text

        let getKind tok =
            let (kind, _) = tok
            kind

        let current () =
            List.tryHead tokens
            |> Option.defaultValue (TokenKind.EndOfFile, "")

        let lookingAtAny kinds =
            let currentKind = current () |> getKind
            List.contains currentKind kinds

        let lookingAt kind =
            let currentKind = current () |> getKind
            currentKind = kind

        let bump () =
            let tok = current ()

            tokens <-
                match tokens with
                | _ :: tail -> tail
                | [] -> []

            tok

        let skipWs (builder: GreenNodeBuilder) =
            while lookingAtAny [ TokenKind.Newline; TokenKind.Whitespace] do
                builder.Token(AstKind.WHITESPACE |> astToGreen, bump () |> getText)

        let skipWsNoNl (builder: GreenNodeBuilder) =
            while lookingAt TokenKind.Whitespace do
                builder.Token(AstKind.WHITESPACE |> astToGreen, bump () |> getText)

        let expect (builder: GreenNodeBuilder) token syntax =
            skipWs builder

            if lookingAt token then
                builder.Token(syntax |> astToGreen, bump () |> getText)
            else
                builder.Token(AstKind.ERROR |> astToGreen, "")

            skipWsNoNl builder

        let parseError (builder: GreenNodeBuilder) =
            builder.Token(AstKind.ERROR |> astToGreen, bump () |> getText)

        let rec parseUse (builder: GreenNodeBuilder) =
            builder.StartNode(AstKind.USE |> astToGreen)
            expect builder TokenKind.Ident AstKind.IDENT
            builder.FinishNode()

        and parseAbstraction (builder: GreenNodeBuilder) =
            builder.StartNode(AstKind.ABSTRACTION |> astToGreen)
            expect builder TokenKind.Lambda AstKind.LAMBDA
            expect builder TokenKind.Ident AstKind.IDENT
            expect builder TokenKind.Dot AstKind.DOT
            parseExpression builder
            builder.FinishNode()

        and parseExpression (builder: GreenNodeBuilder) =
            skipWs builder

            let mark = builder.Mark()

            match current () |> getKind with
            | Ident -> parseUse builder
            | Lambda -> parseAbstraction builder
            | _ -> parseError builder

            skipWsNoNl builder

            if not <| lookingAtAny [ TokenKind.EndOfFile; TokenKind.Newline ] then
                parseExpression builder
                builder.ApplyMark(mark, AstKind.APPLICATION |> astToGreen)

        let parseProgram () =

            let builder = GreenNodeBuilder()

            while not <| lookingAt TokenKind.EndOfFile do
                parseExpression builder

            expect builder TokenKind.EndOfFile AstKind.END

            builder.BuildRoot(AstKind.PROGRAM |> astToGreen)

        parseProgram () |> SyntaxNode.CreateRoot

module Ast =

    open Parse
    open Firethorn

    type IdentifierSyntax(token: SyntaxToken) =

        static member Cast(token: SyntaxToken) =
            if token.Kind |> astFromGreen = AstKind.IDENT then
                Some(IdentifierSyntax(token))
            else
                None

        member _.Name = token.Green.Text

    type UseSyntax(syntax: SyntaxNode) =

        static member Cast(node: SyntaxNode) =
            if node.Kind |> astFromGreen = AstKind.USE then
                Some(UseSyntax(node))
            else
                None

        member _.Ident =
            syntax.ChildrenWithTokens()
            |> Seq.tryPick (
                NodeOrToken.asToken
                >> (Option.bind IdentifierSyntax.Cast)
            )

    and AbstractionSyntax(syntax: SyntaxNode) =

        static member Cast(node: SyntaxNode) =
            if node.Kind |> astFromGreen = AstKind.ABSTRACTION then
                Some(AbstractionSyntax(node))
            else
                None

        member _.Binding =
            syntax.ChildrenWithTokens()
            |> Seq.tryPick (
                NodeOrToken.asToken
                >> (Option.bind IdentifierSyntax.Cast)
            )

        member _.Body =
            syntax.Children()
            |> Seq.choose ExpressionSyntax.Cast
            |> Seq.tryHead

    and ExpressionSyntax =
        | Abstraction of AbstractionSyntax
        | Use of UseSyntax

        static member Cast(node: SyntaxNode) =
            (UseSyntax.Cast node |> Option.map (Use))
            |> Option.orElseWith
                (fun () ->
                    AbstractionSyntax.Cast node
                    |> Option.map Abstraction)

    type ProgramSyntax(syntax: SyntaxNode) =

        static member Cast(syntax: SyntaxNode) =
            if syntax.Kind |> astFromGreen = AstKind.PROGRAM then
                Some(ProgramSyntax(syntax))
            else
                None

        member _.Expressions =
            syntax.Children()
            |> Seq.choose (ExpressionSyntax.Cast)

let kindToName = Parse.astFromGreen >> sprintf "%A"

let prettyPrint tree =

    let mutable indent = 0

    let printIndent () =
        String.init indent (fun _ -> "  ") |> printf "%s"

    printfn "Red tree structure:"

    tree
    |> Walk.walk
    |> Seq.iter
        (function
        | EnterNode n ->
            printIndent ()
            indent <- indent + 1
            printfn "%s@%O" (n.Kind |> kindToName) n.Range
        | LeaveNode n -> indent <- indent - 1
        | OnToken t ->
            printIndent ()
            printfn "%s@%O '%s'" (t.Kind |> kindToName) t.Range t.Green.Text)

    /// Re-construct the origional text by walking the tree and concatenating
    /// the tokens' text.
    let builder = StringBuilder()

    tree
    |> Walk.walk
    |> Seq.iter
        (function
        | EnterNode _ -> ()
        | LeaveNode _ -> ()
        | OnToken t -> bprintf builder "%s" t.Green.Text)

    printfn "Origional text: %s" (string builder)


    let typedTree = (Ast.ProgramSyntax.Cast(tree).Value)
    printfn "%A - %d expressions" typedTree (typedTree.Expressions |> Seq.length)

[<EntryPoint>]
let main argv =
    argv |> Array.iter (Parse.parse >> prettyPrint)
    0 // return an integer exit code
