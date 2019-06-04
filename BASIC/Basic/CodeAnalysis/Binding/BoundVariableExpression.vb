Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundVariableExpression
    Inherits BoundExpression

    Sub New(name As String, type As Type)
      Me.Name = name
      Me.Type = type
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.VariableExpression
    Public Overrides ReadOnly Property Type As Type
    Public ReadOnly Property Name As String

  End Class

End Namespace