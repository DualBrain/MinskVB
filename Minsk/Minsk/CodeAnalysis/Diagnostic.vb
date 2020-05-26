Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.CodeAnalysis

  Public NotInheritable Class Diagnostic

    Sub New(location As TextLocation, message As String)
      Me.Location = location
      Me.Message = message
    End Sub

    Public ReadOnly Property Location As TextLocation
    Public ReadOnly Property Message As String

    Public Overrides Function ToString() As String
      Return Message
    End Function

  End Class

End Namespace