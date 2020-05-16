Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class NameExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(tree As SyntaxTree, identifierToken As SyntaxToken)
      MyBase.New(tree)
      Me.IdentifierToken = identifierToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.NameExpression
    Public ReadOnly Property IdentifierToken As SyntaxToken

  End Class

End Namespace