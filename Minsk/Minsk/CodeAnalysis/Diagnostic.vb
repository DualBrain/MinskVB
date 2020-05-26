Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.CodeAnalysis

  Public NotInheritable Class Diagnostic

    Sub New(location As TextLocation, message As String, isError As Boolean)
      Me.Location = location
      Me.Message = message
      Me.IsError = isError
      Me.IsWarning = Not isError
    End Sub

    Public ReadOnly Property Location As TextLocation
    Public ReadOnly Property Message As String
    Public ReadOnly Property IsError As Boolean
    Public ReadOnly Property IsWarning As Boolean

    Public Overrides Function ToString() As String
      Return Message
    End Function

    Public Shared Function [Error](location As TextLocation, message As String) As Diagnostic
      Return New Diagnostic(location, message, isError:=True)
    End Function

    Public Shared Function Warning(location As TextLocation, message As String) As Diagnostic
      Return New Diagnostic(location, message, isError:=False)
    End Function

  End Class

End Namespace