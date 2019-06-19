Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Symbols

  Public NotInheritable Class ParameterSymbol
    Inherits LocalVariableSymbol

    Sub New(name As String, type As TypeSymbol)
      MyBase.New(name, True, type)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Parameter

  End Class

End Namespace