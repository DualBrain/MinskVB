Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundUnaryExpression
    Inherits BoundExpression

    Sub New(op As BoundUnaryOperator, operand As BoundExpression)
      Me.Op = op
      Me.Operand = operand
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.UnaryExpression
    Public Overrides ReadOnly Property Type As Type
      Get
        Return Me.Operand.Type
      End Get
    End Property
    Public ReadOnly Property Op As BoundUnaryOperator
    Public ReadOnly Property Operand As BoundExpression

  End Class

End Namespace