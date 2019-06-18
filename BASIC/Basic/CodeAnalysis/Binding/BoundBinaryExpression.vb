Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundBinaryExpression
    Inherits BoundExpression

    Sub New(left As BoundExpression, op As BoundBinaryOperator, right As BoundExpression)
      Me.Left = left
      Me.Op = op
      Me.Right = right
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.BinaryExpression
    Public Overrides ReadOnly Property Type As TypeSymbol
      Get
        Return Me.Op.Type
      End Get
    End Property
    Public ReadOnly Property Left As BoundExpression
    Public ReadOnly Property Op As BoundBinaryOperator
    Public ReadOnly Property Right As BoundExpression

  End Class

End Namespace