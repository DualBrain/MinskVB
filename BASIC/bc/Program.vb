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
Imports Basic.CodeAnalysis.Binding
Imports Basic.CodeAnalysis.Syntax
Imports System.Console

Friend Module Program

  Sub Main()

    Dim showTree = False

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
      Dim result = compilation.Evaluate

      Dim diagnostics = result.Diagnostics

      ' Only show the parse tree if we have enabled doing so.
      If showTree Then
        Dim color = Console.ForegroundColor
        Console.ForegroundColor = ConsoleColor.DarkGray
        PrettyPrint(tree.Root)
        Console.ResetColor()
      End If

      If Not diagnostics.Any Then
        ' No errors detected, attemp to evaluate (execute).
        WriteLine(result.Value)
      Else
        ' We have errors, so don't try to evaluate (execute).
        Console.ForegroundColor = ConsoleColor.DarkRed
        For Each diagnostic In diagnostics
          WriteLine(diagnostic)
        Next
        Console.ResetColor()
      End If

    Loop

  End Sub

  Sub PrettyPrint(node As SyntaxNode, Optional indent As String = "", Optional isLast As Boolean = True)

    Dim marker = If(isLast, "└──", "├──")

    Write(indent)
    Write(marker)

    Write($"{node.Kind}")

    If TryCast(node, SyntaxToken)?.Value IsNot Nothing Then
      Write(" ")
      Write(DirectCast(node, SyntaxToken).Value)
    End If

    WriteLine()

    indent += If(isLast, "   ", "│  ")

    Dim lastChild = node.GetChildren.LastOrDefault

    For Each child In node.GetChildren
      PrettyPrint(child, indent, child Is lastChild)
    Next

  End Sub

End Module

