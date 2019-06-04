Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundAssignmentExpression
    Inherits BoundExpression

    Sub New(name As String, expression As BoundExpression)
      Me.Name = name
      Me.expression = expression
      Me.Type = expression.Type
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.AssignmentExpression
    Public Overrides ReadOnly Property Type As Type
    Public ReadOnly Property Name As String
    Public ReadOnly Property Expression As BoundExpression

  End Class

End Namespace