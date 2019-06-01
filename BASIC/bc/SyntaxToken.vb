Option Explicit On ' Variables must be "declared".
Option Strict On ' They must have a "type".
Option Infer On ' Where we can, the "type" can be inferred.
Imports bc

' This is a .NET Core 3.0 VB.NET project.
' I added the above Option's to enable how I like
' to write software.

' You can find the code at 
' https://github.com/dualbrain/basic
' Feel free to join in and contribute!


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