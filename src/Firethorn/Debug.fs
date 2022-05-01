namespace Firethorn.Red

open Firethorn

open System.IO
open System.Text

module Debug =

    /// Formatter for syntax kinds. Used by debug dumping to format out the
    /// opaque kinds on each syntax element in a meaningful way.
    type public KindFormatter = TextWriter -> SyntaxKind -> unit

    let rawFormatter writer kind = Printf.fprintf writer "%A" kind

    /// Function to generate a kind formatter that maps kinds using a `mapper`
    /// function ready for printing.
    let public mappedFormatter mapper =
        fun writer kind -> Printf.fprintf writer "%A" (kind |> mapper)

    /// Debug Dump to a Given Text Writer
    ///
    /// This is the base debug output function for syntax elements. Use this to
    /// write a debug implementation to some nominal text writer location.
    ///
    /// For more convenient output see `debugDump` which writes to stdout and
    /// `debugToString` which returns the formatted string representaiton.
    let public debugDumpTo (outputSink: TextWriter) (greenToAst: KindFormatter) =
        let mutable nesting = 0

        let indent () =
            for _ in 1..nesting do
                Printf.fprintf outputSink "  "

        let onEvent =
            function
            | WalkEvent.EnterNode node ->
                indent ()
                Printf.fprintfn outputSink "%a: (%O)" greenToAst node.Kind node.Range
                nesting <- nesting + 1
            | WalkEvent.LeaveNode node -> nesting <- nesting - 1
            | WalkEvent.OnToken token ->
                indent ()
                Printf.fprintfn outputSink "%a: (%O) %A" greenToAst token.Kind token.Range token.Green.Text

        Walk.walk >> Seq.iter onEvent

    /// Write the debug representation for a node to the current console out
    let public debugDump = debugDumpTo System.Console.Out

    /// Write the debug representation for a node to the current console out
    /// using the raw kind formatter.
    let public debugDumpRaw = debugDump rawFormatter

    /// Return a debug string representation
    let public debugToString kindFormatter element =
        let sb = StringBuilder()
        use sw = new StringWriter(sb)
        debugDumpTo sw kindFormatter element
        sb.ToString()

    /// Return a debug string representaiton using raw numeric kinds
    let public debugToStringRaw = debugToString rawFormatter
