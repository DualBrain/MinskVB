Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class ForStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, keyword As SyntaxToken, identifier As SyntaxToken, equalsToken As SyntaxToken, lowerBound As ExpressionSyntax, toKeyword As SyntaxToken, upperBound As ExpressionSyntax, body As StatementSyntax)
      MyBase.New(tree)
      Me.Keyword = keyword
      Me.Identifier = identifier
      Me.EqualsToken = equalsToken
      Me.LowerBound = lowerBound
      Me.ToKeyword = toKeyword
      Me.UpperBound = upperBound
      Me.Body = body
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ForStatement
    Public ReadOnly Property Keyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property EqualsToken As SyntaxToken
    Public ReadOnly Property LowerBound As ExpressionSyntax
    Public ReadOnly Property ToKeyword As SyntaxToken
    Public ReadOnly Property UpperBound As ExpressionSyntax
    Public ReadOnly Property Body As StatementSyntax

  End Class

End Namespace