Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Text
Imports Basic.IO
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

  <MetaCommand("showTree", "Shows the parse tree")>
  Private Sub EvaluateShowTree()
    m_showTree = Not m_showTree
    Console.WriteLine(If(m_showTree, "Showing parse trees.", "Now showing parse trees."))
  End Sub

  <MetaCommand("showProgram", "Shows the bound tree")>
  Private Sub EvaluateShowProgram()
    m_showProgram = Not m_showProgram
    Console.WriteLine(If(m_showProgram, "Showing bound tree.", "Now showing bound tree."))
  End Sub

  <MetaCommand("cls", "Clears the screen")>
  Protected Sub EvaluateCls()
    Clear()
  End Sub

  <MetaCommand("reset", "Clears all previous submissions")>
  Protected Sub EvaluateReset()
    m_previous = Nothing
    m_variables.Clear()
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

    ' Use Members because we need to exclude the EndOfFileToken.
    If tree.Root.Members.Last.GetLastToken.IsMissing Then
      Return False
    End If

    Return True

  End Function

  Protected Overrides Sub EvaluateSubmission(text As String)

    Dim tree = SyntaxTree.Parse(text)

    'If Not isBlank AndAlso tree.Diagnostics.Any Then
    '  Continue Do
    'End If

    Dim compilation = If(m_previous Is Nothing, New Compilation(tree), m_previous.ContinueWith(tree))

    If m_showTree Then
      Dim color = Console.ForegroundColor
      ForegroundColor = DarkGray
      tree.Root.WriteTo(Console.Out)
      ResetColor()
    End If

    If m_showProgram Then
      compilation.EmitTree(Console.Out)
      ResetColor()
    End If

    Dim result = compilation.Evaluate(m_variables)

    If Not result.Diagnostics.Any Then

      If result.Value IsNot Nothing Then
        ForegroundColor = White
        WriteLine(result.Value)
        ResetColor()
      End If

      m_previous = compilation

    Else

      Console.Out.WriteDiagnostics(result.Diagnostics)

    End If

  End Sub

End Class

'Friend NotInheritable Class TextSpanComparer
'  Implements IComparer(Of TextSpan)

'  Public Function Compare(x As TextSpan, y As TextSpan) As Integer Implements IComparer(Of TextSpan).Compare
'    Dim cmp = x.Start - y.Start
'    If cmp = 0 Then
'      cmp = x.Length - y.Length
'    End If
'    Return cmp
'  End Function

'End Class