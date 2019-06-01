Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis

  Class SyntaxToken
    Inherits SyntaxNode

    Sub New(kind As SyntaxKind, position As Integer, text As String, value As Object)
      Me.Kind = kind
      Me.Position = position
      Me.Text = text
      Me.Value = value
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind
    Public ReadOnly Property Position As Integer
    Public ReadOnly Property Text As String
    Public ReadOnly Property Value As Object

    Public Overrides Function GetChildren() As IEnumerable(Of SyntaxNode)
      Return Enumerable.Empty(Of SyntaxNode)
    End Function

  End Class

End Namespace