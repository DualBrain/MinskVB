Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public NotInheritable Class WhileStatementSyntax
    Inherits StatementSyntax

    Sub New(whileKeyword As SyntaxToken, condition As ExpressionSyntax, body As StatementSyntax)
      Me.WhileKeyword = whileKeyword
      Me.Condition = condition
      Me.Body = body
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.WhileStatement
    Public ReadOnly Property WhileKeyword As SyntaxToken
    Public ReadOnly Property Condition As ExpressionSyntax
    Public ReadOnly Property Body As StatementSyntax

  End Class

End Namespace