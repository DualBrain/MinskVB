Option Explicit On
Option Strict On
Option Infer On

Imports System.CodeDom.Compiler
Imports System.IO
Imports System.Runtime.CompilerServices
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis
Imports System.Console
Imports System.ConsoleColor
Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.IO

  Public Module TextWriterExtensions

    <Extension>
    Private Function IsConsoleOut(writer As TextWriter) As Boolean

      If writer Is Console.Out Then
        Return True
      End If

      If TypeOf writer Is IndentedTextWriter Then
        Dim iw = DirectCast(writer, IndentedTextWriter)
        If iw.InnerWriter.IsConsoleOut() Then
          Return True
        End If
      End If

      Return False

    End Function

    <Extension>
    Private Sub SetForeground(writer As TextWriter, color As ConsoleColor)
      If writer.IsConsoleOut() Then
        Console.ForegroundColor = color
      End If
    End Sub

    <Extension>
    Private Sub ResetColor(writer As TextWriter)
      If writer.IsConsoleOut() Then
        Console.ResetColor()
      End If
    End Sub

    <Extension>
    Public Sub WriteKeyword(writer As TextWriter, kind As SyntaxKind)
      writer.WriteKeyword(SyntaxFacts.GetText(kind))
    End Sub

    <Extension>
    Public Sub WriteKeyword(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.Blue)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteIdentifier(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.DarkYellow)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteNumber(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.Cyan)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteString(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.Magenta)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteSpace(writer As TextWriter)
      writer.WritePunctuation(" ")
    End Sub


    <Extension>
    Public Sub WritePunctuation(writer As TextWriter, kind As SyntaxKind)
      writer.WritePunctuation(SyntaxFacts.GetText(kind))
    End Sub

    <Extension>
    Public Sub WritePunctuation(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.DarkGray)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteDiagnostics(writer As TextWriter, diagnostics As IEnumerable(Of Diagnostic))

      ' We have errors, so don't try to evaluate (execute).
      For Each diagnostic In diagnostics.OrderBy(Function(diag) diag.Location.FileName).
                                         ThenBy(Function(diag) diag.Location.Span).
                                         ThenBy(Function(diag) diag.Location.Span.Length)

        Dim text = diagnostic.Location.Text
        Dim filename = diagnostic.Location.FileName
        Dim startLine = diagnostic.Location.StartLine + 1
        Dim startCharacter = diagnostic.Location.StartCharacter + 1
        Dim endLine = diagnostic.Location.EndLine + 1
        Dim endCharacter = diagnostic.Location.EndCharacter + 1

        Dim span = diagnostic.Location.Span
        Dim lineIndex = text.GetLineIndex(span.Start)
        Dim line = text.Lines(lineIndex)

        Dim character = span.Start - line.Start + 1

        ' An extra line before for clarity...
        WriteLine()

        ForegroundColor = DarkRed
        Write($"{filename}({startLine},{startCharacter},{endLine},{endCharacter}): ")
        WriteLine(diagnostic)
        Console.ResetColor()

        Dim prefixSpan = TextSpan.FromBounds(line.Start, span.Start)
        Dim suffixSpan = TextSpan.FromBounds(span.End, line.End)

        Dim prefix = text.ToString(prefixSpan)
        Dim er = text.ToString(span)
        Dim suffix = text.ToString(suffixSpan)

        ' Write the prefix in "normal" text...
        Write($"    {prefix}")
        ' Write the error portion in red...
        ForegroundColor = DarkRed
        Write(er)
        Console.ResetColor()
        ' Write the rest of the line.
        WriteLine(suffix)

      Next

      ' An extra line at the end for clarity.
      WriteLine()

    End Sub

  End Module

  'Friend NotInheritable Class TextSpanComparer
  '  Implements IComparer(Of TextSpan)

  '  Public Function Compare(x As TextSpan, y As TextSpan) As Integer Implements IComparer(Of TextSpan).Compare
  '    Dim cmp = x.Start - y.Start
  '    If cmp = 0 Then
  '      cmp = x.Length - y.Length
  '    End If
  '    Return cmp
  '  End Function

  'End Class

End Namespace