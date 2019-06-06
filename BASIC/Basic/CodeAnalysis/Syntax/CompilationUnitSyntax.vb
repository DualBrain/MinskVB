Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public NotInheritable Class CompilationUnitSyntax
    Inherits SyntaxNode

    Sub New(expression As ExpressionSyntax, endOfFileToken As SyntaxToken)
      Me.Expression = expression
      Me.EndOfFileToken = endOfFileToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CompilationUnit
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property EndOfFileToken As SyntaxToken

  End Class

End Namespace