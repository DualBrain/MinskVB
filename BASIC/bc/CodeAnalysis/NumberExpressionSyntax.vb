Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis

  NotInheritable Class NumberExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(numberToken As SyntaxToken)
      Me.NumberToken = numberToken
    End Sub

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Me.NumberToken
    End Function

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.NumberExpression
    Public ReadOnly NumberToken As SyntaxToken

  End Class

End Namespace