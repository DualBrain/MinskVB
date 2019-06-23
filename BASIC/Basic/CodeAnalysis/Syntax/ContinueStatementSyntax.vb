Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Friend Class ContinueStatementSyntax
    Inherits StatementSyntax

    Public Sub New(keyword As SyntaxToken)
      Me.Keyword = keyword
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind
      Get
        Return SyntaxKind.ContinueStatement
      End Get
    End Property
    Public ReadOnly Property Keyword() As SyntaxToken

  End Class

End Namespace