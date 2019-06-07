﻿Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public NotInheritable Class ExpressionStatementSyntax
    Inherits StatementSyntax

    ' a = 10
    ' a + 1
    ' a++
    ' M()

    Sub New(expression As ExpressionSyntax)
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ExpressionStatement
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace