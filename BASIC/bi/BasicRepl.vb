Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Text
Imports Basic.IO
Imports System.Collections.Immutable
Imports System.Console
Imports System.ConsoleColor
Imports System.IO

Friend NotInheritable Class BasicRepl
  Inherits Repl

  Private Shared m_loadingSubmission As Boolean
  Private Shared ReadOnly m_emptyCompilation As Compilation = Compilation.CreateScript(Nothing)
  Private m_previous As Compilation = Nothing
  Private m_showTree As Boolean = False
  Private m_showProgram As Boolean = False
  Private ReadOnly m_variables As New Dictionary(Of VariableSymbol, Object)

  Sub New()
    LoadSubmissions()
  End Sub

  Private NotInheritable Class RenderState

    Public Sub New(text As SourceText, tokens As ImmutableArray(Of SyntaxToken))
      Me.Text = text
      Me.Tokens = tokens
    End Sub

    Public ReadOnly Property Text As SourceText
    Public ReadOnly Property Tokens As ImmutableArray(Of SyntaxToken)

  End Class

  Protected Overrides Function RenderLine(lines As IReadOnlyList(Of String), lineIndex As Integer, state As Object) As Object

    Dim renderState As RenderState

    If state Is Nothing Then
      Dim text = String.Join(Environment.NewLine, lines)
      Dim st = SourceText.From(text)
      Dim tokens = SyntaxTree.ParseTokens(st)
      renderState = New RenderState(st, tokens)
    Else
      renderState = CType(state, RenderState)
    End If

    Dim lineSpan = renderState.Text.Lines(lineIndex).Span

    For Each token In renderState.Tokens

      If Not lineSpan.OverlapsWith(token.Span) Then Continue For

      Dim tokenStart = Math.Max(token.Span.Start, lineSpan.Start)
      Dim tokenEnd = Math.Min(token.Span.End, lineSpan.End)
      Dim tokenSpan = TextSpan.FromBounds(tokenStart, tokenEnd)
      Dim tokenText = renderState.Text.ToString(tokenSpan)

      Dim isKeyword = token.Kind.ToString.EndsWith("Keyword")
      Dim isNumber = token.Kind = SyntaxKind.NumberToken
      Dim isIdentifier = token.Kind = SyntaxKind.IdentifierToken
      Dim isString = token.Kind = SyntaxKind.StringToken
      Dim isComment = token.Kind = SyntaxKind.SingleLineCommentToken

      If isKeyword Then
        ForegroundColor = Blue
      ElseIf isIdentifier Then
        ForegroundColor = DarkYellow
      ElseIf isNumber Then
        ForegroundColor = Cyan
      ElseIf isString Then
        ForegroundColor = Magenta
      ElseIf isComment Then
        ForegroundColor = Green
      Else
        ForegroundColor = DarkGray
      End If
      Write(tokenText)
      ResetColor()

    Next

    Return renderState

  End Function

  <MetaCommand("exit", "Exits the REPL")>
  Protected Sub EvaluateExit()
    Environment.Exit(0)
  End Sub

  <MetaCommand("cls", "Clears the screen")>
  Protected Sub EvaluateCls()
    Clear()
  End Sub

  <MetaCommand("reset", "Clears all previous submissions")>
  Protected Sub EvaluateReset()
    m_previous = Nothing
    m_variables.Clear()
    ClearSubmissions()
  End Sub

  <MetaCommand("showTree", "Shows the parse tree")>
  Protected Sub EvaluateShowTree()
    m_showTree = Not m_showTree
    Console.WriteLine(If(m_showTree, "Showing parse trees.", "Now showing parse trees."))
  End Sub

  <MetaCommand("showProgram", "Shows the bound tree")>
  Protected Sub EvaluateShowProgram()
    m_showProgram = Not m_showProgram
    Console.WriteLine(If(m_showProgram, "Showing bound tree.", "Now showing bound tree."))
  End Sub

  <MetaCommand("load", "Loads a script file")>
  Protected Sub EvaluateLoad(path As String)

    path = IO.Path.GetFullPath(path)
    If Not System.IO.File.Exists(path) Then
      Console.ForegroundColor = ConsoleColor.Red
      Console.WriteLine($"error: file does not exist '{path}'")
      Console.ResetColor()
      Return
    End If

    Dim text = IO.File.ReadAllText(path)
    EvaluateSubmission(text)

  End Sub

  <MetaCommand("ls", "Lists all symbols")>
  Protected Sub EvaluateLs()

    'If m_previous Is Nothing Then
    '  Return
    'End If
    'Dim symbols = m_previous.GetSymbols.OrderBy(Function(s) s.Kind).ThenBy(Function(s) s.Name)
    Dim compilation = If(m_previous, m_emptyCompilation)
    Dim symbols = compilation.GetSymbols.OrderBy(Function(s) s.Kind).ThenBy(Function(s) s.Name)

    For Each symbol In symbols
      symbol.WriteTo(Console.Out)
      Console.WriteLine()
    Next

  End Sub

  <MetaCommand("dump", "Shows bound tree of a given function")>
  Protected Sub EvaluateDump(functionName As String)

    'If m_previous Is Nothing Then
    '  Return
    'End If
    'Dim symbol = m_previous.GetSymbols.OfType(Of FunctionSymbol).SingleOrDefault(Function(f) f.Name = functionName)
    Dim compilation = If(m_previous, m_emptyCompilation)
    Dim symbol = compilation.GetSymbols.OfType(Of FunctionSymbol).SingleOrDefault(Function(f) f.Name = functionName)

    If symbol Is Nothing Then
      Console.ForegroundColor = ConsoleColor.Red
      Console.WriteLine($"error: function '{functionName}' does not exist")
      Console.ResetColor()
      Return
    End If

    'm_previous.EmitTree(symbol, Console.Out)
    compilation.EmitTree(symbol, Console.Out)

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
    Dim lastMember = tree.Root.Members.LastOrDefault
    If lastMember Is Nothing OrElse lastMember.GetLastToken.IsMissing Then
      Return False
    End If

    Return True

  End Function

  Protected Overrides Sub EvaluateSubmission(text As String)

    Dim tree = SyntaxTree.Parse(text)
    Dim compilation = Basic.CodeAnalysis.Compilation.CreateScript(m_previous, tree)

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

      SaveSubmission(text)

    Else

      Console.Out.WriteDiagnostics(result.Diagnostics)

    End If

  End Sub

  Private Shared Function GetSubmissionsDirectory() As String
    Dim localAppData = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData)
    Dim submissionsDirectory = Path.Combine(localAppData, "Minsk", "Submissions")
    Return submissionsDirectory
  End Function

  Private Sub LoadSubmissions()

    Dim submissionsDirectory = GetSubmissionsDirectory()
    If Not Directory.Exists(submissionsDirectory) Then Return
    Dim files = Directory.GetFiles(submissionsDirectory).OrderBy(Function(f) f).ToArray
    If files.Length = 0 Then Return

    Console.ForegroundColor = ConsoleColor.DarkGray
    Console.WriteLine($"Loaded {files.Length} submission(s)")
    Console.ResetColor()

    m_loadingSubmission = True

    For Each File In files
      Dim text = IO.File.ReadAllText(File)
      EvaluateSubmission(text)
    Next

    m_loadingSubmission = False

  End Sub

  Private Sub ClearSubmissions()
    Dim dir = GetSubmissionsDirectory()
    If Directory.Exists(dir) Then
      Directory.Delete(dir, recursive:=True)
    End If
  End Sub

  Private Sub SaveSubmission(text As String)
    If m_loadingSubmission Then Return
    Dim submissionsDirectory = GetSubmissionsDirectory()
    Directory.CreateDirectory(submissionsDirectory)
    Dim count = Directory.GetFiles(submissionsDirectory).Length
    Dim name = $"submission{count:0000}"
    Dim fileName = Path.Combine(submissionsDirectory, name)
    File.WriteAllText(fileName, text)
  End Sub

End Class