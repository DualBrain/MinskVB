﻿Option Explicit On
Option Strict On
Option Infer On

Imports System.CodeDom.Compiler
Imports System.IO
Imports System.Runtime.CompilerServices

Namespace Global.Basic.IO

  Friend Module TextWriterExtensions

    <Extension>
    Public Function IsConsoleOut(writer As TextWriter) As Boolean

      If writer Is Console.Out Then
        Return True
      End If

      If TypeOf writer Is IndentedTextWriter Then
        Dim iw = DirectCast(writer, IndentedTextWriter)
        If iw.InnerWriter.IsConsoleOut() Then
          Return True
        End If
      End If

      Return False

    End Function

    <Extension>
    Public Sub SetForeground(writer As TextWriter, color As ConsoleColor)
      If writer.IsConsoleOut() Then
        Console.ForegroundColor = color
      End If
    End Sub

    <Extension>
    Public Sub ResetColor(writer As TextWriter)
      If writer.IsConsoleOut() Then
        Console.ResetColor()
      End If
    End Sub

    <Extension>
    Public Sub WriteKeyword(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.Blue)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteIdentifier(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.DarkYellow)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteNumber(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.Cyan)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteString(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.Magenta)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WritePunctuation(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.DarkGray)
      writer.Write(text)
      writer.ResetColor()
    End Sub

  End Module

End Namespace