Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLabel

    Friend Sub New(name As String)
      Me.Name = name
    End Sub

    Public ReadOnly Property Name As String

    Public Overrides Function ToString() As String
      Return Me.Name
    End Function

  End Class

End Namespace