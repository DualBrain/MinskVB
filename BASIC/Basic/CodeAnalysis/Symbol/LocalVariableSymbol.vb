Option Explicit On
Option Strict On
Option Infer On
Imports Basic.CodeAnalysis.Binding

Namespace Global.Basic.CodeAnalysis.Symbols

  Public Class LocalVariableSymbol
    Inherits VariableSymbol

    Friend Sub New(name As String, isReadOnly As Boolean, type As TypeSymbol, constant As BoundConstant)
      MyBase.New(name, isReadOnly, type, constant)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.LocalVariable

  End Class

End Namespace