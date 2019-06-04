Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis

  Public NotInheritable Class VariableSymbol

    Friend Sub New(name As String, type As Type)
      Me.Name = name
      Me.Type = type
    End Sub

    Public ReadOnly Property Name As String
    Public ReadOnly Property Type As Type

  End Class

End Namespace