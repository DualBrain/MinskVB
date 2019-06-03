Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public NotInheritable Class ParenExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(openParenToken As SyntaxToken, expression As ExpressionSyntax, closeParenToken As SyntaxToken)
      Me.OpenParenToken = openParenToken
      Me.Expression = expression
      Me.CloseParenToken = closeParenToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ParenExpression
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property CloseParenToken As SyntaxToken

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Me.OpenParenToken
      Yield Me.Expression
      Yield Me.CloseParenToken
    End Function

  End Class

End Namespace