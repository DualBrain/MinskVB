Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLiteralExpression
    Inherits BoundExpression

    Sub New(value As Object)
      'Me.Value = value
      If TypeOf value Is Boolean Then
        Type = TypeSymbol.Bool
      ElseIf TypeOf Value Is Integer Then
        Type = TypeSymbol.Int
      ElseIf TypeOf value Is String Then
        Type = TypeSymbol.String
      Else
        Throw New Exception($"Unexpected literal '{value}' of type {value.GetType}.")
      End If
      ConstantValue = New BoundConstant(value)
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.LiteralExpression
    Public Overrides ReadOnly Property Type As TypeSymbol
    Public ReadOnly Property Value As Object
      Get
        Return ConstantValue.Value
      End Get
    End Property
    Public Overrides ReadOnly Property ConstantValue As BoundConstant

  End Class

End Namespace