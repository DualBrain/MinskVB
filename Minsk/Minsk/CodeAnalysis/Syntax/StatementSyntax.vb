Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public MustInherit Class StatementSyntax
    Inherits SyntaxNode

    Friend Sub New(tree As SyntaxTree)
      MyBase.New(tree)
    End Sub

  End Class

End Namespace