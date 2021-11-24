namespace Firethorn

/// A position in source text.
type TextLength = int

/// A range of positions in source text.
type TextRange =
    { Start: TextLength
      End: TextLength }

    // Custom string representation for text ranges.
    override self.ToString() = sprintf "%d..%d" self.Start self.End
