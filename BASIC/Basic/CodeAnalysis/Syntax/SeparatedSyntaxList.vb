Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable

Namespace Global.Basic.CodeAnalysis.Syntax

  Public MustInherit Class SeparatedSyntaxList

    Public MustOverride Function GetWithSeparators() As ImmutableArray(Of SyntaxNode)

  End Class

  Public NotInheritable Class SeparatedSyntaxList(Of T As SyntaxNode)
    Inherits SeparatedSyntaxList
    Implements IEnumerable(Of T)

    Private ReadOnly m_nodesAndSeparators As ImmutableArray(Of SyntaxNode)

    Sub New(nodesAndSeparators As ImmutableArray(Of SyntaxNode))
      Me.m_nodesAndSeparators = nodesAndSeparators
    End Sub

    Public ReadOnly Property Count As Integer
      Get
        Return (Me.m_nodesAndSeparators.Length + 1) \ 2
      End Get
    End Property

    Default Public ReadOnly Property Item(index As Integer) As T
      Get
        Return CType(Me.m_nodesAndSeparators(index * 2), T)
      End Get
    End Property

    Public Function GetSeparator(index As Integer) As SyntaxToken
      If index = Me.Count - 1 Then Return Nothing
      Return CType(Me.m_nodesAndSeparators(index * 2 + 1), SyntaxToken)
    End Function

    Public Overrides Function GetWithSeparators() As ImmutableArray(Of SyntaxNode)
      Return Me.m_nodesAndSeparators
    End Function

    Public Iterator Function GetEnumerator() As IEnumerator(Of T) Implements IEnumerable(Of T).GetEnumerator
      For i = 0 To Me.Count - 1
        Yield Me(i)
      Next
    End Function

    Private Function IEnumerable_GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
      Return Me.GetEnumerator()
    End Function

  End Class

End Namespace