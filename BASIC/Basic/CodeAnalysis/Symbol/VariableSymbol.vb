Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Symbols

  Public NotInheritable Class VariableSymbol
    Inherits Symbol

    Friend Sub New(name As String, isReadOnly As Boolean, type As Type)
      MyBase.New(name)
      Me.IsReadOnly = isReadOnly
      Me.Type = type
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Variable
    Public ReadOnly Property IsReadOnly As Boolean
    Public ReadOnly Property Type As Type

  End Class

End Namespace