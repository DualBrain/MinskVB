Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundVariableExpression
    Inherits BoundExpression

    Sub New(variable As VariableSymbol)
      Me.Variable = variable
      Me.Type = variable.Type
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.VariableExpression
    Public Overrides ReadOnly Property Type As Type
    Public ReadOnly Property Variable As VariableSymbol

  End Class

End Namespace