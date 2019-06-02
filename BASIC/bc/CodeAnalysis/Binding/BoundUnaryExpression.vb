Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundUnaryExpression
    Inherits BoundExpression

    Sub New(operatorKind As BoundUnaryOperatorKind, operand As BoundExpression)
      Me.OperatorKind = operatorKind
      Me.Operand = operand
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.UnaryExpression
    Public Overrides ReadOnly Property Type As Type
      Get
        Return Me.Operand.Type
      End Get
    End Property
    Public ReadOnly Property OperatorKind As BoundUnaryOperatorKind
    Public ReadOnly Property Operand As BoundExpression

  End Class

End Namespace