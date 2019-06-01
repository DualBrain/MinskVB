Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis

  NotInheritable Class SyntaxTree

    Sub New(diagnostics As IEnumerable(Of String), root As ExpressionSyntax, endOfFileToken As SyntaxToken)
      Me.Diagnostics = diagnostics.ToArray
      Me.Root = root
      Me.EndOfFileToken = endOfFileToken
    End Sub

    Public ReadOnly Property Diagnostics As IReadOnlyList(Of String)
    Public ReadOnly Property Root As ExpressionSyntax
    Public ReadOnly Property EndOfFileToken As SyntaxToken

    Public Shared Function Parse(text As String) As SyntaxTree
      Dim parser = New Parser(text)
      Return parser.Parse
    End Function

  End Class

End Namespace