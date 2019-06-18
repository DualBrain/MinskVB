Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Symbols

  Public MustInherit Class Symbol

    Protected Friend Sub New(name As String)
      Me.Name = name
    End Sub

    Public MustOverride ReadOnly Property Kind As SymbolKind
    Public ReadOnly Property Name As String

    Public Overrides Function ToString() As String
      Return Me.Name
    End Function

  End Class

End Namespace