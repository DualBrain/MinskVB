Option Explicit On
Option Strict On
Option Infer On

' You can find the latest version at 
' https://github.com/dualbrain/basic
' Feel free to join in and contribute!
' You can watch me stream at
' https://twitch.tv/gotbasic
' Want to learn more about BASIC...
' https://gotbasic.com

Imports Basic.CodeAnalysis
Imports Basic.CodeAnalysis.Syntax
Imports System.Console

Friend Module Program

  Sub Main()

    Dim showTree = False
    Dim variables = New Dictionary(Of VariableSymbol, Object)

    Do

      Write("> ")

      Dim line = ReadLine()

      ' Handle "special commands / circumstances" within the "REPL".

      If String.IsNullOrWhiteSpace(line) Then
        Continue Do
      End If

      Select Case line.ToLower
        Case "option tree on"
          showTree = True : Continue Do
        Case "option tree off"
          showTree = False : Continue Do
        Case "cls"
          Clear() : Continue Do
        Case "exit"
          Exit Do
        Case Else
      End Select

      ' Otherwise, attempt to parse what was entered...

      Dim tree = SyntaxTree.Parse(line)
      Dim compilation = New Compilation(tree)
      Dim result = compilation.Evaluate(variables)

      Dim diagnostics = result.Diagnostics

      ' Only show the parse tree if we have enabled doing so.

      If showTree Then
        Dim color = Console.ForegroundColor
        Console.ForegroundColor = ConsoleColor.DarkGray
        tree.Root.WriteTo(Console.Out)
        Console.ResetColor()
      End If

      If Not diagnostics.Any Then
        ' No errors detected, attempt to evaluate (execute).
        WriteLine(result.Value)
      Else

        Dim text = tree.Text

        ' We have errors, so don't try to evaluate (execute).
        For Each diagnostic In diagnostics

          Dim lineIndex = text.GetLineIndex(diagnostic.Span.Start)
          Dim lineNumber = lineIndex + 1
          Dim character = diagnostic.Span.Start - text.Lines(lineIndex).Span.Start + 1

          ' An extra line before for clarity...
          WriteLine()

          Console.ForegroundColor = ConsoleColor.DarkRed
          Write($"({lineNumber}, {character}): ")
          WriteLine(diagnostic)
          Console.ResetColor()

          'TODO: (1+2  <--- crashes
          Dim prefix = line.Substring(0, diagnostic.Span.Start)
          Dim er = line.Substring(diagnostic.Span.Start, diagnostic.Span.Length)
          Dim suffix = line.Substring(diagnostic.Span.End)

          ' Write the prefix in "normal" text...
          Write($"    {prefix}")
          ' Write the error portion in red...
          Console.ForegroundColor = ConsoleColor.DarkRed
          Write(er)
          Console.ResetColor()
          ' Write the rest of the line.
          WriteLine(suffix)

        Next

        ' An extra line at the end for clarity.
        WriteLine()

      End If

    Loop

  End Sub

End Module

