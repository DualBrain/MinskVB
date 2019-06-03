Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis
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

  End Structure

End Namespace