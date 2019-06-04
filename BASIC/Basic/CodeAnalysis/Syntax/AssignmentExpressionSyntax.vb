Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public NotInheritable Class AssignmentExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(identifierToken As SyntaxToken, equalsToken As SyntaxToken, expression As ExpressionSyntax)
      Me.IdentifierToken = identifierToken
      Me.EqualsToken = equalsToken
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.AssignmentExpression
    Public ReadOnly Property IdentifierToken As SyntaxToken
    Public ReadOnly Property EqualsToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Me.IdentifierToken
      Yield Me.EqualsToken
      Yield Me.Expression
    End Function

  End Class

End Namespace