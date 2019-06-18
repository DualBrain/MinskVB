Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Text
Imports System.Console
Imports System.ConsoleColor

Friend NotInheritable Class BasicRepl
  Inherits Repl

  Private m_previous As Compilation = Nothing
  Private m_showTree As Boolean = False
  Private m_showProgram As Boolean = False
  Private ReadOnly m_variables As New Dictionary(Of VariableSymbol, Object)

  Protected Overrides Sub RenderLine(line As String)

    Dim tokens = SyntaxTree.ParseTokens(line)

    For Each token In tokens

      Dim isKeyword = token.Kind.ToString.EndsWith("Keyword")
      Dim isNumber = token.Kind = SyntaxKind.NumberToken
      Dim isIdentifier = token.Kind = SyntaxKind.IdentifierToken
      Dim isString = token.Kind = SyntaxKind.StringToken

      If isKeyword Then
        ForegroundColor = Blue
      ElseIf isIdentifier Then
        ForegroundColor = DarkYellow
      ElseIf isNumber Then
        ForegroundColor = Cyan
      ElseIf isString Then
        ForegroundColor = Magenta
      Else
        ForegroundColor = DarkGray
      End If
      Write(token.Text)
      ResetColor()

    Next

  End Sub

  Protected Overrides Sub EvaluateMetaCommand(input As String)
    Select Case input.ToLower
      Case "#new"
        Me.m_previous = Nothing
      Case "#toggle tree"
        Me.m_showTree = Not Me.m_showTree
      Case "#toggle program"
        Me.m_showProgram = Not Me.m_showProgram
      Case "#cls"
        Clear()
      Case Else
        MyBase.EvaluateMetaCommand(input)
    End Select
  End Sub

  Protected Overrides Function IsCompleteSubmission(text As String) As Boolean

    If String.IsNullOrEmpty(text) Then Return True

    Dim lastTwoLinesAreBlank = text.Split(Environment.NewLine).
                                        Reverse().
                                        TakeWhile(Function(s) String.IsNullOrEmpty(s)).
                                        Take(2).
                                        Count() = 2

    If lastTwoLinesAreBlank Then
      Return True
    End If

    Dim tree = SyntaxTree.Parse(text)

    ' Use Statement because we need to exclude the EndOfFileToken.
    If tree.Root.Statement.GetLastToken.IsMissing Then
      Return False
    End If

    Return True

  End Function

  Protected Overrides Sub EvaluateSubmission(text As String)
    Dim tree = SyntaxTree.Parse(text)

    'If Not isBlank AndAlso tree.Diagnostics.Any Then
    '  Continue Do
    'End If

    Dim compilation = If(Me.m_previous Is Nothing, New Compilation(tree), Me.m_previous.ContinueWith(tree))

    If Me.m_showTree Then
      Dim color = Console.ForegroundColor
      ForegroundColor = DarkGray
      tree.Root.WriteTo(Console.Out)
      ResetColor()
    End If

    If Me.m_showProgram Then
      compilation.EmitTree(Console.Out)
      ResetColor()
    End If

    Dim result = compilation.Evaluate(Me.m_variables)

    If Not result.Diagnostics.Any Then

      ' No errors detected, attempt to evaluate (execute).

      ForegroundColor = White
      WriteLine(result.Value)
      ResetColor()

      Me.m_previous = compilation

    Else

      ' We have errors, so don't try to evaluate (execute).
      For Each diagnostic In result.Diagnostics

        Dim lineIndex = tree.Text.GetLineIndex(diagnostic.Span.Start)
        Dim lineNumber = lineIndex + 1
        Dim line = tree.Text.Lines(lineIndex)
        Dim character = diagnostic.Span.Start - line.Start + 1

        ' An extra line before for clarity...
        WriteLine()

        ForegroundColor = DarkRed
        Write($"({lineNumber}, {character}): ")
        WriteLine(diagnostic)
        ResetColor()

        Dim prefixSpan = TextSpan.FromBounds(line.Start, diagnostic.Span.Start)
        Dim suffixSpan = TextSpan.FromBounds(diagnostic.Span.End, line.End)

        Dim prefix = tree.Text.ToString(prefixSpan)
        Dim er = tree.Text.ToString(diagnostic.Span)
        Dim suffix = tree.Text.ToString(suffixSpan)

        ' Write the prefix in "normal" text...
        Write($"    {prefix}")
        ' Write the error portion in red...
        ForegroundColor = DarkRed
        Write(er)
        Console.ResetColor()
        ' Write the rest of the line.
        WriteLine(suffix)

      Next

      ' An extra line at the end for clarity.
      WriteLine()

    End If

  End Sub

End Class