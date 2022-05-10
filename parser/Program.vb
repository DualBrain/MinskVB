Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Authoring
Imports Basic.IO
Imports System.Console
Imports System.ConsoleColor
Imports System.IO

Module Program
  Sub Main(args As String())

    Dim text = <code>
print("hello world")
x = x + 1
               </code>.Value

    Dim tree = SyntaxTree.Parse(text)

    PrettyPrint(Console.Out, tree.Root)

    For Each entry In tree.Diagnostics
      Console.WriteLine(entry)
    Next

  End Sub

  Private Sub PrettyPrint(writer As TextWriter, node As SyntaxNode, Optional indent As String = "", Optional isLast As Boolean = True)

    ' HACK: node should never be null, but that's tracked by #141
    If node Is Nothing Then Return

    Dim isToConsole = writer Is Console.Out
    Dim token = TryCast(node, SyntaxToken)

    If token IsNot Nothing Then
      For Each trivia In token.LeadingTrivia

        If isToConsole Then ForegroundColor = DarkGray

        writer.Write(indent)
        writer.Write("├──")

        If isToConsole Then ForegroundColor = DarkGreen

        writer.WriteLine($"L: {trivia.Kind}")

      Next
    End If

    Dim hasTrailingTrivia = token IsNot Nothing AndAlso token.TrailingTrivia.Any
    Dim tokenMarker = If(Not hasTrailingTrivia AndAlso isLast, "└──", "├──")

    If isToConsole Then ForegroundColor = DarkGray

    writer.Write(indent)
    writer.Write(tokenMarker)

    If isToConsole Then
      ForegroundColor = If(TypeOf node Is SyntaxToken, Blue, Cyan)
    End If
    writer.Write($"{node.Kind}")

    If token IsNot Nothing AndAlso token.Value IsNot Nothing Then
      writer.Write(" ")
      writer.Write(token.Value)
    End If

    If isToConsole Then
      ResetColor()
    End If

    writer.WriteLine()

    If token IsNot Nothing Then
      For Each trivia In token.TrailingTrivia

        Dim isLastTrailingTrivia = trivia Is token.TrailingTrivia.Last
        Dim triviaMarker = If(isLast AndAlso isLastTrailingTrivia, "└──", "├──")

        If isToConsole Then ForegroundColor = DarkGray

        writer.Write(indent)
        writer.Write(triviaMarker)

        If isToConsole Then ForegroundColor = DarkGreen

        writer.WriteLine($"T: {trivia.Kind}")

      Next
    End If

    indent += If(isLast, "   ", "│  ")

    Dim lastChild = node.GetChildren.LastOrDefault

    For Each child In node.GetChildren
      PrettyPrint(writer, child, indent, child Is lastChild)
    Next

  End Sub

End Module
