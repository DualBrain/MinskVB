Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis

  Public NotInheritable Class LiteralExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(literalToken As SyntaxToken)
      Me.LiteralToken = literalToken
    End Sub

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Me.LiteralToken
    End Function

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LiteralExpression
    Public ReadOnly LiteralToken As SyntaxToken

  End Class

End Namespace