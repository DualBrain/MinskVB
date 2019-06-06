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
Imports Basic.CodeAnalysis.Text
Imports System.Console
Imports System.Text

Friend Module Program

  Sub Main()

    Dim showTree = False
    Dim variables = New Dictionary(Of VariableSymbol, Object)
    Dim textBuilder = New StringBuilder

    Do

      If (textBuilder.Length = 0) Then
        Console.Write("> ")
      Else
        Console.Write("| ")
      End If

      Dim input = ReadLine()
      Dim isBlank = String.IsNullOrWhiteSpace(input)

      ' Handle "special commands / circumstances" within the "REPL".

      If textBuilder.Length = 0 Then

        If isBlank Then
          Exit Do
        End If

        Select Case input.ToLower
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

      End If

      ' Otherwise, attempt to parse what was entered...

      textBuilder.AppendLine(input)
      Dim text = textBuilder.ToString

      Dim tree = SyntaxTree.Parse(text)

      If Not isBlank AndAlso tree.Diagnostics.Any Then
        Continue Do
      End If

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

        ' We have errors, so don't try to evaluate (execute).
        For Each diagnostic In diagnostics

          Dim lineIndex = tree.Text.GetLineIndex(diagnostic.Span.Start)
          Dim lineNumber = lineIndex + 1
          Dim line = tree.Text.Lines(lineIndex)
          Dim character = diagnostic.Span.Start - line.Start + 1

          ' An extra line before for clarity...
          WriteLine()

          Console.ForegroundColor = ConsoleColor.DarkRed
          Write($"({lineNumber}, {character}): ")
          WriteLine(diagnostic)
          Console.ResetColor()

          Dim prefixSpan = TextSpan.FromBounds(line.Start, diagnostic.Span.Start)
          Dim suffixSpan = TextSpan.FromBounds(diagnostic.Span.End, line.End)

          Dim prefix = tree.Text.ToString(prefixSpan)
          Dim er = tree.Text.ToString(diagnostic.Span)
          Dim suffix = tree.Text.ToString(suffixSpan)

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

      textBuilder.Clear()

    Loop

  End Sub

End Module

