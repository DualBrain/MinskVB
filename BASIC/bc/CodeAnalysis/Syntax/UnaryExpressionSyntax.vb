Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax
  Public NotInheritable Class UnaryExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(operatorToken As SyntaxToken, operand As ExpressionSyntax)
      Me.OperatorToken = operatorToken
      Me.Operand = operand
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.UnaryExpression
    Public ReadOnly Property OperatorToken As SyntaxToken
    Public ReadOnly Property Operand As ExpressionSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Me.OperatorToken
      Yield Me.Operand
    End Function

  End Class

End Namespace