Option Explicit On
Option Strict On
Option Infer On

Imports System.Reflection

Namespace Global.Basic.CodeAnalysis.Syntax

  Public MustInherit Class SyntaxNode

    Public MustOverride ReadOnly Property Kind() As SyntaxKind

    Public Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)

      Dim properties = Me.GetType.GetProperties(BindingFlags.[Public] Or BindingFlags.Instance)

      For Each p In properties
        If GetType(SyntaxNode).IsAssignableFrom(p.PropertyType) Then
          Dim child = DirectCast(p.GetValue(Me), SyntaxNode)
          Yield child
        ElseIf GetType(IEnumerable(Of SyntaxNode)).IsAssignableFrom(p.PropertyType) Then
          Dim children = DirectCast(p.GetValue(Me), IEnumerable(Of SyntaxNode))
          For Each child In children
            Yield child
          Next
        End If
      Next

    End Function

  End Class

End Namespace