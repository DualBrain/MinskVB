Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Text

  Public Structure TextSpan

    Sub New(start As Integer, length As Integer)
      Me.Start = start
      Me.Length = length
    End Sub

    Public ReadOnly Property Start As Integer
    Public ReadOnly Property Length As Integer

    Public ReadOnly Property [End] As Integer
      Get
        Return Me.Start + Me.Length
      End Get
    End Property

    Public Shared Function FromBounds(start As Integer, [end] As Integer) As TextSpan
      Dim length = [end] - start
      Return New TextSpan(start, length)
    End Function

    Public Overrides Function ToString() As String
      Return $"{Me.Start}...{Me.End}"
    End Function

  End Structure

End Namespace