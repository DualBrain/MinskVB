Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Symbols

  Public MustInherit Class VariableSymbol
    Inherits Symbol

    Friend Sub New(name As String, isReadOnly As Boolean, type As TypeSymbol)
      MyBase.New(name)
      Me.IsReadOnly = isReadOnly
      Me.Type = type
    End Sub

    Public ReadOnly Property IsReadOnly As Boolean
    Public ReadOnly Property Type As TypeSymbol

  End Class

End Namespace