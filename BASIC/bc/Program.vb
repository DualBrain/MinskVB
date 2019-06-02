Option Explicit On ' Variables must be "declared".
Option Strict On ' They must have a "type".
Option Infer On ' Where we can, the "type" can be inferred.

' This is a .NET Core 3.0 VB.NET project.
' I added the above Option's to enable how I like
' to write software.

' You can find the code at 
' https://github.com/dualbrain/basic
' Feel free to join in and contribute!

' 1 + 2 * 3
'
' Could be...
'
'   +
'  / \ 
' 1   *
'    / \
'   2   3
'
' Desired...
'
'     *
'    / \ 
'   +   3
'  / \
' 1   2

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
      Dim binder = New Binder()
      Dim boundExpression = binder.BindExpression(tree.Root)
      Dim diagnostics = tree.Diagnostics.Concat(binder.Diagnostics).ToArray

      ' Only show the parse tree if we have enabled doing so.
      If showTree Then
        Dim color = Console.ForegroundColor
        Console.ForegroundColor = ConsoleColor.DarkGray
        PrettyPrint(tree.Root)
        Console.ResetColor()
      End If

      If Not diagnostics.Any Then
        ' No errors detected, attemp to evaluate (execute).
        Dim e = New Evaluator(boundExpression)
        Dim result = e.Evaluate
        WriteLine(result)
      Else
        ' We have errors, so don't try to evaluate (execute).
        Console.ForegroundColor = ConsoleColor.DarkRed
        For Each diagnostic In tree.Diagnostics
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

