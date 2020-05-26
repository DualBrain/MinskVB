Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class DoWhileStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, doKeyword As SyntaxToken, body As StatementSyntax, whileKeyword As SyntaxToken, condition As ExpressionSyntax)
      MyBase.New(tree)
      Me.DoKeyword = doKeyword
      Me.Body = body
      Me.WhileKeyword = whileKeyword
      Me.Condition = condition
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DoWhileStatement
    Public ReadOnly Property DoKeyword As SyntaxToken
    Public ReadOnly Property Body As StatementSyntax
    Public ReadOnly Property WhileKeyword As SyntaxToken
    Public ReadOnly Property Condition As ExpressionSyntax

  End Class

End Namespace