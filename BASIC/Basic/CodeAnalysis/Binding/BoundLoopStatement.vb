Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend MustInherit Class BoundLoopStatement
    Inherits BoundStatement

    Protected Sub New(breakLabel As BoundLabel, continueLabel As BoundLabel)
      Me.BreakLabel = breakLabel
      Me.ContinueLabel = continueLabel
    End Sub

    Public ReadOnly Property BreakLabel() As BoundLabel
    Public ReadOnly Property ContinueLabel() As BoundLabel

  End Class

End Namespace