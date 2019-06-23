Option Explicit On
Option Strict On
Option Infer On

Imports System.IO

Namespace Global.Basic.CodeAnalysis.Symbols

  Public MustInherit Class Symbol

    Protected Friend Sub New(name As String)
      Me.Name = name
    End Sub

    Public MustOverride ReadOnly Property Kind As SymbolKind
    Public ReadOnly Property Name As String

    Public Sub WriteTo(writer As TextWriter)
      SymbolPrinter.WriteTo(Me, writer)
    End Sub

    Public Overrides Function ToString() As String
      Using writer = New StringWriter()
        Me.WriteTo(writer)
        Return writer.ToString()
      End Using
    End Function

  End Class

End Namespace