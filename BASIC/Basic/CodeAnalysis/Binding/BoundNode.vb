Option Explicit On
Option Strict On
Option Infer On

Imports System.IO

Namespace Global.Basic.CodeAnalysis.Binding

  Friend MustInherit Class BoundNode

    MustOverride ReadOnly Property Kind As BoundNodeKind

    Public Overrides Function ToString() As String
      Using writer = New StringWriter
        Me.WriteTo(writer)
        Return writer.ToString
      End Using
    End Function

  End Class

End Namespace