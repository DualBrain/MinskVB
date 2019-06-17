Option Explicit On
Option Strict On
Option Infer On

Imports System.Console
Imports System.ConsoleColor
Imports System.Text

Friend MustInherit Class Repl

  Private ReadOnly m_textBuilder As New StringBuilder

  Public Sub Run()

    Do

      ForegroundColor = Green

      If (Me.m_textBuilder.Length = 0) Then
        Write("» ")
      Else
        Write("· ")
      End If

      ResetColor()

      Dim input = ReadLine()
      Dim isBlank = String.IsNullOrWhiteSpace(input)

      If Me.m_textBuilder.Length = 0 Then

        If isBlank Then
          Exit Do
        ElseIf input.StartsWith("#") Then
          Me.EvaluateMetaCommand(input)
          Continue Do
        End If

      End If

      Me.m_textBuilder.AppendLine(input)
      Dim text = Me.m_textBuilder.ToString
      If Not Me.IsCompleteSubmission(text) Then
        Continue Do
      End If

      Me.EvaluateSubmission(text)

      Me.m_textBuilder.Clear()

    Loop

  End Sub

  Protected Overridable Sub EvaluateMetaCommand(input As String)
    ForegroundColor = Red
    WriteLine($"Invalid command {input}.")
    ResetColor()
  End Sub

  Protected MustOverride Function IsCompleteSubmission(text As String) As Boolean

  Protected MustOverride Sub EvaluateSubmission(text As String)

End Class