Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public NotInheritable Class NameExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(identifierToken As SyntaxToken)
      Me.IdentifierToken = identifierToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.NameExpression
    Public ReadOnly Property IdentifierToken As SyntaxToken

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Me.IdentifierToken
    End Function

  End Class

End Namespace