Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundConditionalGotoStatement
    Inherits BoundStatement

    Sub New(label As LabelSymbol, condition As BoundExpression, Optional jumpIfFalse As Boolean = False)
      Me.Label = label
      Me.Condition = condition
      Me.JumpIfFalse = jumpIfFalse
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ConditionalGotoStatement
    Public ReadOnly Property Label As LabelSymbol
    Public ReadOnly Property Condition As BoundExpression
    Public ReadOnly Property JumpIfFalse As Boolean

  End Class

End Namespace