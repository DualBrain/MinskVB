Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLiteralExpression
    Inherits BoundExpression

    Sub New(value As Object)
      Me.Value = value
      If TypeOf Me.Value Is Boolean Then
        Me.Type = TypeSymbol.Bool
      ElseIf TypeOf Me.Value Is Integer Then
        Me.Type = TypeSymbol.Int
      ElseIf TypeOf value Is String Then
        Me.Type = TypeSymbol.String
      Else
        Throw New Exception($"Unexpected literal '{value}' of type {value.GetType}.")
      End If
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.LiteralExpression
    Public Overrides ReadOnly Property Type As TypeSymbol
    Public ReadOnly Property Value As Object

  End Class

End Namespace