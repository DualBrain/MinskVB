Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLiteralExpression
    Inherits BoundExpression

    Sub New(value As Object)
      Me.Value = value
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.LiteralExpression
    Public Overrides ReadOnly Property Type As Type
      Get
        Return Me.Value.GetType
      End Get
    End Property
    Public ReadOnly Property Value As Object

  End Class

End Namespace