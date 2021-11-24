open Firethorn.Red

module Parse =

    open System

    open Firethorn
    open Firethorn.Green

    type TokenKind =
        | Lambda
        | Dot
        | Ident
        | Whitespace
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
        | PROGRAM = 102

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

        let bump () =
            let tok = current ()

            tokens <-
                match tokens with
                | _ :: tail -> tail
                | [] -> []

            tok

        let expect token syntax =
            if current () |> getKind = token then
                GreenToken.Create(syntax |> astToGreen, bump () |> getText)
            else
                GreenToken.Create(AstKind.ERROR |> astToGreen, "")

        let parseError () =
            GreenToken.Create(AstKind.ERROR |> astToGreen, bump () |> getText)
            |> Token

        let rec parseUse () =
            GreenNode.Create(AstKind.USE |> astToGreen, [ expect TokenKind.Ident AstKind.IDENT |> Token ])
            |> Node

        and parseAbstraction () =
            let mutable children = [ expect TokenKind.Lambda AstKind.LAMBDA |> Token ]

            let maybeWs () =
                if current () |> getKind = TokenKind.Whitespace then
                    children <- 
                    (expect TokenKind.Whitespace AstKind.WHITESPACE |> Token)
                    :: children
            maybeWs ()
            children <- 
                  (expect TokenKind.Ident AstKind.IDENT |> Token) :: children
            maybeWs ()
            children <- 
                  (expect TokenKind.Dot AstKind.DOT |> Token) :: children
            maybeWs ()
            children <-
                parseExpression () :: children

            GreenNode.Create(
                AstKind.ABSTRACTION |> astToGreen,
                List.rev children
            )
            |> Node

        and parseExpression () =
            match current () |> getKind with
            | Ident -> parseUse ()
            | Lambda -> parseAbstraction ()
            | _ -> parseError ()

        let parseProgram () =
            let mutable elements = []

            while current () |> getKind <> TokenKind.EndOfFile do
                elements <- List.append elements [ parseExpression () ]

            GreenNode.Create(
                AstKind.PROGRAM |> astToGreen,
                List.append elements [ expect TokenKind.EndOfFile AstKind.END |> Token ]
            )

        parseProgram () |> SyntaxNode.CreateRoot

let kindToName = Parse.astFromGreen >> sprintf "%A"

let prettyPrint tree =
    let mutable indent = 0

    let printIndent () =
        String.init indent (fun _ -> "  ") |> printf "%s"

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

    tree
    |> Walk.walk
    |> Seq.iter 
        (function
        | EnterNode _ -> ()
        | LeaveNode _ -> ()
        | OnToken t ->
            printf "%s" t.Green.Text)

[<EntryPoint>]
let main argv =
    argv |> Array.iter (Parse.parse >> prettyPrint)
    0 // return an integer exit code
