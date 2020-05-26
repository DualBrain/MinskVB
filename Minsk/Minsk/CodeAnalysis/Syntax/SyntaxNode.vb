Option Explicit On
Option Strict On
Option Infer On

Imports System.IO
Imports System.Reflection
Imports Basic.CodeAnalysis.Text

Imports System.Console
Imports System.ConsoleColor

Namespace Global.Basic.CodeAnalysis.Syntax

  ' TODO: All custructors should be internal
  Public MustInherit Class SyntaxNode

    Friend Sub New(tree As SyntaxTree)
      SyntaxTree = tree
    End Sub

    Public ReadOnly Property SyntaxTree As SyntaxTree

    Public MustOverride ReadOnly Property Kind() As SyntaxKind

    Public Overridable ReadOnly Property Span() As TextSpan
      Get
        Dim first = GetChildren.First.Span
        Dim last = GetChildren.Last.Span
        Return TextSpan.FromBounds(first.Start, last.End)
      End Get
    End Property

    Public Overridable ReadOnly Property FullSpan() As TextSpan
      Get
        Dim first = GetChildren.First.FullSpan
        Dim last = GetChildren.Last.FullSpan
        Return TextSpan.FromBounds(first.Start, last.End)
      End Get
    End Property

    Public ReadOnly Property Location As TextLocation
      Get
        Return New TextLocation(SyntaxTree.Text, Span)
      End Get
    End Property

    Public MustOverride Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)

    'Public Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)

    '  Dim properties = [GetType].GetProperties(BindingFlags.[Public] Or BindingFlags.Instance)

    '  For Each p In properties
    '    If GetType(SyntaxNode).IsAssignableFrom(p.PropertyType) Then
    '      Dim child = DirectCast(p.GetValue(Me), SyntaxNode)
    '      If child IsNot Nothing Then
    '        Yield child
    '      End If
    '    ElseIf GetType(SeparatedSyntaxList).IsAssignableFrom(p.PropertyType) Then
    '      Dim separatedSyntaxList = DirectCast(p.GetValue(Me), SeparatedSyntaxList)
    '      For Each child In separatedSyntaxList.GetWithSeparators
    '        Yield child
    '      Next
    '    ElseIf GetType(IEnumerable(Of SyntaxNode)).IsAssignableFrom(p.PropertyType) Then
    '      Dim children = DirectCast(p.GetValue(Me), IEnumerable(Of SyntaxNode))
    '      For Each child In children
    '        If child IsNot Nothing Then
    '          Yield child
    '        End If
    '      Next
    '    End If
    '  Next

    'End Function

    Public Function GetLastToken() As SyntaxToken
      If TypeOf Me Is SyntaxToken Then
        Return DirectCast(Me, SyntaxToken)
      End If
      ' A syntax node should always contain at least one token.
      Return GetChildren.Last.GetLastToken
    End Function

    Public Sub WriteTo(writer As TextWriter)
      PrettyPrint(writer, Me)
    End Sub

    Private Shared Sub PrettyPrint(writer As TextWriter, node As SyntaxNode, Optional indent As String = "", Optional isLast As Boolean = True)

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

    Public Overrides Function ToString() As String
      Using writer = New StringWriter
        WriteTo(writer)
        Return writer.ToString
      End Using
    End Function

  End Class

End Namespace