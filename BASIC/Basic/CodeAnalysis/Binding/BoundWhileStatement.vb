Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundDoWhileStatement
    Inherits BoundLoopStatement

    Sub New(body As BoundStatement, condition As BoundExpression, breakLabel As BoundLabel, continuelabel As BoundLabel)
      MyBase.New(breakLabel, continuelabel)
      Me.Body = body
      Me.Condition = condition
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DoWhileStatement
    Public ReadOnly Property Body As BoundStatement
    Public ReadOnly Property Condition As BoundExpression

  End Class

  Friend NotInheritable Class BoundWhileStatement
    Inherits BoundLoopStatement

    Sub New(condition As BoundExpression, body As BoundStatement, breakLabel As BoundLabel, continueLabel As BoundLabel)
      MyBase.New(breakLabel, continueLabel)
      Me.Condition = condition
      Me.Body = body
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.WhileStatement
    Public ReadOnly Property Condition As BoundExpression
    Public ReadOnly Property Body As BoundStatement

  End Class

End Namespace