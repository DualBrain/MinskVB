Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public NotInheritable Class BinaryExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(left As ExpressionSyntax, operatorToken As SyntaxToken, right As ExpressionSyntax)
      Me.Left = left
      Me.OperatorToken = operatorToken
      Me.Right = right
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.BinaryExpression
    Public ReadOnly Property Left As ExpressionSyntax
    Public ReadOnly Property OperatorToken As SyntaxToken
    Public ReadOnly Property Right As ExpressionSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Me.Left
      Yield Me.OperatorToken
      Yield Me.Right
    End Function

  End Class

End Namespace