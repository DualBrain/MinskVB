Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public NotInheritable Class IfStatementSyntax
    Inherits StatementSyntax

    Sub New(ifKeyword As SyntaxToken, condition As ExpressionSyntax, thenStatement As StatementSyntax,
            elseClause As ElseClauseSyntax)
      Me.IfKeyword = ifKeyword
      Me.Condition = condition
      Me.ThenStatement = thenStatement
      Me.ElseClause = elseClause
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.IfStatement
    Public ReadOnly Property IfKeyword As SyntaxToken
    Public ReadOnly Property Condition As ExpressionSyntax
    Public ReadOnly Property ThenStatement As StatementSyntax
    Public ReadOnly Property ElseClause As ElseClauseSyntax

  End Class

End Namespace