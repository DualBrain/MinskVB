Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Symbols

  Public NotInheritable Class TypeSymbol
    Inherits Symbol

    Public Shared ReadOnly Int As New TypeSymbol("int")
    Public Shared ReadOnly Bool As New TypeSymbol("bool")
    Public Shared ReadOnly [String] As New TypeSymbol("string")

    Private Sub New(name As String)
      MyBase.New(name)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Type

  End Class

End Namespace