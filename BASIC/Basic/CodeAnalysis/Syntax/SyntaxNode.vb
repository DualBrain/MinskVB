Option Explicit On
Option Strict On
Option Infer On

Imports System.IO
Imports System.Reflection
Imports Basic.CodeAnalysis.Text

Imports System.Console
Imports System.ConsoleColor

Namespace Global.Basic.CodeAnalysis.Syntax

  Public MustInherit Class SyntaxNode

    Public MustOverride ReadOnly Property Kind() As SyntaxKind

    Public Overridable ReadOnly Property Span() As TextSpan
      Get
        Dim first = Me.GetChildren.First.Span
        Dim last = Me.GetChildren.Last.Span
        Return TextSpan.FromBounds(first.Start, last.End)
      End Get
    End Property

    Public Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)

      Dim properties = Me.GetType.GetProperties(BindingFlags.[Public] Or BindingFlags.Instance)

      For Each p In properties
        If GetType(SyntaxNode).IsAssignableFrom(p.PropertyType) Then
          Dim child = DirectCast(p.GetValue(Me), SyntaxNode)
          If child IsNot Nothing Then
            Yield child
          End If
        ElseIf GetType(IEnumerable(Of SyntaxNode)).IsAssignableFrom(p.PropertyType) Then
          Dim children = DirectCast(p.GetValue(Me), IEnumerable(Of SyntaxNode))
          For Each child In children
            If child IsNot Nothing Then
              Yield child
            End If
          Next
        End If
      Next

    End Function

    Public Function GetLastToken() As SyntaxToken
      If TypeOf Me Is SyntaxToken Then
        Return DirectCast(Me, SyntaxToken)
      End If
      ' A syntax node should always contain at least one token.
      Return Me.GetChildren.Last.GetLastToken
    End Function

    Public Sub WriteTo(writer As TextWriter)
      PrettyPrint(writer, Me)
    End Sub

    Private Shared Sub PrettyPrint(writer As TextWriter, node As SyntaxNode, Optional indent As String = "", Optional isLast As Boolean = True)

      Dim isToConsole = writer Is Console.Out
      Dim marker = If(isLast, "└──", "├──")

      If isToConsole Then
        ForegroundColor = DarkGray
      End If
      writer.Write(indent)
      writer.Write(marker)

      If isToConsole Then
        ForegroundColor = If(TypeOf node Is SyntaxToken, Green, Cyan)
      End If
      writer.Write($"{node.Kind}")

      If TryCast(node, SyntaxToken)?.Value IsNot Nothing Then
        writer.Write(" ")
        writer.Write(DirectCast(node, SyntaxToken).Value)
      End If

      If isToConsole Then
        ResetColor()
      End If

      writer.WriteLine()

      indent += If(isLast, "   ", "│  ")

      Dim lastChild = node.GetChildren.LastOrDefault

      For Each child In node.GetChildren
        PrettyPrint(writer, child, indent, child Is lastChild)
      Next

    End Sub

    Public Overrides Function ToString() As String
      Using writer = New StringWriter
        Me.WriteTo(writer)
        Return writer.ToString
      End Using
    End Function

  End Class

End Namespace