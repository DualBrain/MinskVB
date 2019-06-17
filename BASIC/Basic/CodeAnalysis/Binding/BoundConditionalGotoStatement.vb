Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundConditionalGotoStatement
    Inherits BoundStatement

    Sub New(label As LabelSymbol, condition As BoundExpression, Optional jumpIfTrue As Boolean = True)
      Me.Label = label
      Me.Condition = condition
      Me.JumpIfTrue = jumpIfTrue
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ConditionalGotoStatement
    Public ReadOnly Property Label As LabelSymbol
    Public ReadOnly Property Condition As BoundExpression
    Public ReadOnly Property JumpIfTrue As Boolean

  End Class

End Namespace