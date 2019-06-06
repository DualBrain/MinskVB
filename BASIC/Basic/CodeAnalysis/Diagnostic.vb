Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.CodeAnalysis

  Public NotInheritable Class Diagnostic

    Sub New(span As TextSpan, message As String)
      Me.Span = span
      Me.Message = message
    End Sub

    Public ReadOnly Property Span As TextSpan
    Public ReadOnly Property Message As String

    Public Overrides Function ToString() As String
      Return Me.Message
    End Function

  End Class

End Namespace