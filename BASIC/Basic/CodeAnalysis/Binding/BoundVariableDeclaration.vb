Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundVariableDeclaration
    Inherits BoundStatement

    Sub New(variable As VariableSymbol, initializer As BoundExpression)
      Me.Variable = variable
      Me.Initializer = initializer
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.VariableDeclaration
    Public ReadOnly Property Variable As VariableSymbol
    Public ReadOnly Property Initializer As BoundExpression

  End Class

End Namespace