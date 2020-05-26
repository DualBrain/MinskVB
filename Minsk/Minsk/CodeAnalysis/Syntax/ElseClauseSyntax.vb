Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class ElseClauseSyntax
    Inherits SyntaxNode

    Sub New(tree As SyntaxTree, elseKeyword As SyntaxToken, elseStatement As StatementSyntax)
      MyBase.New(tree)
      Me.ElseKeyword = elseKeyword
      Me.ElseStatement = elseStatement
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ElseClause
    Public ReadOnly Property ElseKeyword As SyntaxToken
    Public ReadOnly Property ElseStatement As StatementSyntax

  End Class

End Namespace