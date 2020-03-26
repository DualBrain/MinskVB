Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public NotInheritable Class GlobalStatementSyntax
    Inherits MemberSyntax

    Sub New(tree As SyntaxTree, statement As StatementSyntax)
      MyBase.New(tree)
      Me.Statement = statement
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.GlobalStatement
    Public ReadOnly Property Statement As StatementSyntax

  End Class

End Namespace