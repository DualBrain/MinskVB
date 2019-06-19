Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Symbols

  Public NotInheritable Class GlobalVariableSymbol
    Inherits VariableSymbol

    Friend Sub New(name As String, isReadOnly As Boolean, type As TypeSymbol)
      MyBase.New(name, isReadOnly, type)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.GlobalVariable

  End Class

End Namespace