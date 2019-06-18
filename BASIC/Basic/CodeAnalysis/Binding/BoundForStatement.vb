Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundForStatement
    Inherits BoundStatement

    Sub New(variable As VariableSymbol, lowerBound As BoundExpression, upperBound As BoundExpression, body As BoundStatement)
      Me.Variable = variable
      Me.LowerBound = lowerBound
      Me.UpperBound = upperBound
      Me.Body = body
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ForStatement
    Public ReadOnly Property Variable As VariableSymbol
    Public ReadOnly Property LowerBound As BoundExpression
    Public ReadOnly Property UpperBound As BoundExpression
    Public ReadOnly Property Body As BoundStatement

  End Class

End Namespace