Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Binding

Namespace Global.Basic.CodeAnalysis.Lowering

  Friend NotInheritable Class Lowerer
    Inherits BoundTreeRewriter

    Private Sub New()
    End Sub

    Public Shared Function Lower(statement As BoundStatement) As BoundStatement
      Dim lowerer = New Lowerer
      Return lowerer.RewriteStatement(statement)
    End Function

  End Class

End Namespace
