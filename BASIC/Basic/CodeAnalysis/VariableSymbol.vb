Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis

  Public NotInheritable Class VariableSymbol

    Friend Sub New(name As String, isReadOnly As Boolean, type As Type)
      Me.Name = name
      Me.IsReadOnly = isReadOnly
      Me.Type = type
    End Sub

    Public ReadOnly Property Name As String
    Public ReadOnly Property IsReadOnly As Boolean
    Public ReadOnly Property Type As Type

  End Class

End Namespace