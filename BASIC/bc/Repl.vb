Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.ObjectModel
Imports System.Collections.Specialized
Imports System.Console
Imports System.ConsoleColor
Imports System.Text
Imports bc

Friend MustInherit Class Repl

  Private ReadOnly m_submissionHistory As New List(Of String)
  Private m_submissionHistoryIndex As Integer
  Private m_done As Boolean = False

  Public Sub Run()
    Do
      Dim text = Me.EditSubmission()
      If String.IsNullOrEmpty(text) Then Return
      If Not text.Contains(Environment.NewLine) AndAlso text.StartsWith("#") Then
        Me.EvaluateMetaCommand(text)
      Else
        Me.EvaluateSubmission(text)
      End If
      Me.m_submissionHistory.Add(text)
      Me.m_submissionHistoryIndex = 0
    Loop
  End Sub

  Private NotInheritable Class SubmissionView

    Private WithEvents SubmissionDocument As ObservableCollection(Of String)
    Private ReadOnly m_cursorTop As Integer
    Private ReadOnly m_lineRenderer As Action(Of String)
    Private m_renderedLineCount As Integer
    Private m_currentLine As Integer
    Private m_currentCharacter As Integer

    Sub New(lineRenderer As Action(Of String), submissionDocument As ObservableCollection(Of String))
      Me.m_lineRenderer = lineRenderer
      Me.SubmissionDocument = submissionDocument
      Me.m_cursorTop = CursorTop
      Me.Render()
    End Sub

    Private Sub CollectionChanged(sender As Object, e As NotifyCollectionChangedEventArgs) Handles SubmissionDocument.CollectionChanged
      Me.Render()
    End Sub

    Private Sub Render()

      'SetCursorPosition(0, Me.m_cursorTop)
      CursorVisible = False

      Dim lineCount = 0

      For Each line In Me.SubmissionDocument

        SetCursorPosition(0, Me.m_cursorTop + lineCount)
        ForegroundColor = Green

        If lineCount = 0 Then
          Write("» ")
        Else
          Write("· ")
        End If

        ResetColor()
        Me.m_lineRenderer(line)
        'Write(line)
        Write(New String(" "c, WindowWidth - line.Length))
        lineCount += 1

      Next

      Dim numberOfBlankLines = Me.m_renderedLineCount - lineCount
      If numberOfBlankLines > 0 Then
        Dim blankLine = New String(" "c, WindowWidth)
        For i = 0 To numberOfBlankLines - 1
          SetCursorPosition(0, Me.m_cursorTop + lineCount + i)
          WriteLine(blankLine)
          'numberOfBlankLines -= 1
        Next
      End If

      Me.m_renderedLineCount = lineCount
      CursorVisible = True
      Me.UpdateCursorPosition()

    End Sub

    Private Sub UpdateCursorPosition()
      SetCursorPosition(2 + Me.CurrentCharacter, Me.m_cursorTop + Me.CurrentLine)
    End Sub

    Public Property CurrentLine As Integer
      Get
        Return Me.m_currentLine
      End Get
      Set
        If Me.m_currentLine <> Value Then
          Me.m_currentLine = Value
          Me.m_currentCharacter = Math.Min(Me.SubmissionDocument(Me.m_currentLine).Length, Me.m_currentCharacter)
          Me.UpdateCursorPosition()
        End If
      End Set
    End Property

    Public Property CurrentCharacter As Integer
      Get
        Return Me.m_currentCharacter
      End Get
      Set
        If Me.m_currentCharacter <> Value Then
          Me.m_currentCharacter = Value
          Me.UpdateCursorPosition()
        End If
      End Set
    End Property

  End Class

  Private Function EditSubmission() As String

    Me.m_done = False

    Dim document = New ObservableCollection(Of String) From {""}
    Dim view = New SubmissionView(AddressOf Me.RenderLine, document)

    While Not Me.m_done
      Dim key = ReadKey(True)
      Me.HandleKey(key, document, view)
    End While

    view.CurrentLine = document.Count - 1
    view.CurrentCharacter = document(view.CurrentLine).Length
    WriteLine()

    Return String.Join(Environment.NewLine, document)

  End Function

  Private Sub HandleKey(key As ConsoleKeyInfo, document As ObservableCollection(Of String), view As SubmissionView)
    If key.Modifiers = 0 Then
      Select Case key.Key
        Case ConsoleKey.Escape : Me.HandleEscape(document, view)
        Case ConsoleKey.Enter : Me.HandleEnter(document, view)
        Case ConsoleKey.LeftArrow : Me.HandleLeftArrow(document, view)
        Case ConsoleKey.RightArrow : Me.HandleRightArrow(document, view)
        Case ConsoleKey.UpArrow : Me.HandleUpArrow(document, view)
        Case ConsoleKey.DownArrow : Me.HandleDownArrow(document, view)
        Case ConsoleKey.Backspace : Me.HandleBackspace(document, view)
        Case ConsoleKey.Delete : Me.HandleDelete(document, view)
        Case ConsoleKey.Home : Me.HandleHome(document, view)
        Case ConsoleKey.End : Me.HandleEnd(document, view)
        Case ConsoleKey.Tab : Me.HandleTab(document, view)
        Case ConsoleKey.PageUp : Me.HandlePageUp(document, view)
        Case ConsoleKey.PageDown : Me.HandlePageDown(document, view)
        Case Else
      End Select
    ElseIf key.Modifiers = ConsoleModifiers.Control Then
      Select Case key.Key
        Case ConsoleKey.Enter
          Me.HandleControlEnter(document, view)
        Case Else
      End Select
    End If
    If key.KeyChar >= " "c Then
      Me.HandleTyping(document, view, key.KeyChar.ToString)
    End If
  End Sub

  Private Sub HandleEscape(document As ObservableCollection(Of String), view As SubmissionView)
    document(view.CurrentLine) = String.Empty
    view.CurrentCharacter = 0
  End Sub

  Private Sub HandleEnter(document As ObservableCollection(Of String), view As SubmissionView)
    Dim submissionText = String.Join(Environment.NewLine, document)
    If submissionText.StartsWith("#") OrElse Me.IsCompleteSubmission(submissionText) Then
      Me.m_done = True
      Return
    End If
    InsertLine(document, view)
  End Sub

  Private Sub HandleControlEnter(document As ObservableCollection(Of String), view As SubmissionView)
    InsertLine(document, view)
  End Sub

  Private Shared Sub InsertLine(document As ObservableCollection(Of String), view As SubmissionView)
    Dim remainder = document(view.CurrentLine).Substring(view.CurrentCharacter)
    document(view.CurrentLine) = document(view.CurrentLine).Substring(0, view.CurrentCharacter)
    Dim lineIndex = view.CurrentLine + 1
    document.Insert(lineIndex, remainder)
    view.CurrentCharacter = 0
    view.CurrentLine = lineIndex
  End Sub

  Private Sub HandleLeftArrow(document As ObservableCollection(Of String), view As SubmissionView)
    If view.CurrentCharacter > 0 Then
      view.CurrentCharacter -= 1
    End If
  End Sub

  Private Sub HandleRightArrow(document As ObservableCollection(Of String), view As SubmissionView)
    Dim line = document(view.CurrentLine)
    If view.CurrentCharacter <= line.Length - 1 Then
      view.CurrentCharacter += 1
    End If
  End Sub

  Private Sub HandleUpArrow(document As ObservableCollection(Of String), view As SubmissionView)
    If view.CurrentLine > 0 Then
      view.CurrentLine -= 1
    End If
  End Sub

  Private Sub HandleDownArrow(document As ObservableCollection(Of String), view As SubmissionView)
    If view.CurrentLine < document.Count - 1 Then
      view.CurrentLine += 1
    End If
  End Sub

  Private Sub HandleBackspace(document As ObservableCollection(Of String), view As SubmissionView)
    Dim start = view.CurrentCharacter
    If start = 0 Then
      If view.CurrentLine = 0 Then Return
      Dim currentLine = document(view.CurrentLine)
      Dim previousLine = document(view.CurrentLine - 1)
      document.RemoveAt(view.CurrentLine)
      view.CurrentLine -= 1
      document(view.CurrentLine) = previousLine & currentLine
      view.CurrentCharacter = previousLine.Length
    Else
      Dim lineIndex = view.CurrentLine
      Dim line = document(lineIndex)
      Dim before = line.Substring(0, start - 1)
      Dim after = line.Substring(start)
      document(lineIndex) = before & after
      view.CurrentCharacter -= 1
    End If
  End Sub

  Private Sub HandleDelete(document As ObservableCollection(Of String), view As SubmissionView)
    Dim lineIndex = view.CurrentLine
    Dim line = document(lineIndex)
    Dim start = view.CurrentCharacter
    If start >= line.Length Then
      If view.CurrentLine = document.Count - 1 Then
        Return
      End If
      Dim nextLine = document(view.CurrentLine + 1)
      document(view.CurrentLine) &= nextLine
      document.RemoveAt(view.CurrentLine + 1)
      Return
    End If
    Dim before = line.Substring(0, start)
    Dim after = line.Substring(start + 1)
    document(lineIndex) = before & after
  End Sub

  Private Sub HandleHome(document As ObservableCollection(Of String), view As SubmissionView)
    view.CurrentCharacter = 0
  End Sub

  Private Sub HandleEnd(document As ObservableCollection(Of String), view As SubmissionView)
    view.CurrentCharacter = document(view.CurrentLine).Length
  End Sub

  Private Sub HandlePageUp(document As ObservableCollection(Of String), view As SubmissionView)
    Me.m_submissionHistoryIndex -= 1
    If Me.m_submissionHistoryIndex < 0 Then
      Me.m_submissionHistoryIndex = Me.m_submissionHistory.Count - 1
    End If
    Me.UpdateDocumentFromHistory(document, view)
  End Sub

  Private Sub HandlePageDown(document As ObservableCollection(Of String), view As SubmissionView)
    If Me.m_submissionHistory.Count = 0 Then Return
    Me.m_submissionHistoryIndex += 1
    If Me.m_submissionHistoryIndex > Me.m_submissionHistory.Count - 1 Then
      Me.m_submissionHistoryIndex = 0
    End If
    Me.UpdateDocumentFromHistory(document, view)
  End Sub

  Private Sub UpdateDocumentFromHistory(document As ObservableCollection(Of String), view As SubmissionView)
    If Me.m_submissionHistory.Count = 0 Then Return
    document.Clear()
    Dim historyItem = Me.m_submissionHistory(Me.m_submissionHistoryIndex)
    Dim lines = historyItem.Split(Environment.NewLine)
    For Each line In lines
      document.Add(line)
    Next
    view.CurrentLine = document.Count - 1
    view.CurrentCharacter = document(view.CurrentLine).Length
  End Sub

  Private Sub HandleTab(document As ObservableCollection(Of String), view As SubmissionView)
    Const TAB_WIDTH As Integer = 2
    Dim start = view.CurrentCharacter
    Dim remainingSpaces = TAB_WIDTH - start Mod TAB_WIDTH
    Dim line = document(view.CurrentLine)
    document(view.CurrentLine) = line.Insert(start, New String(" "c, remainingSpaces))
    view.CurrentCharacter += remainingSpaces
  End Sub

  Private Sub HandleTyping(document As ObservableCollection(Of String), view As SubmissionView, text As String)
    Dim lineIndex = view.CurrentLine
    Dim start = view.CurrentCharacter
    document(lineIndex) = document(lineIndex).Insert(start, text)
    view.CurrentCharacter += text.Length
  End Sub

  Protected Overridable Sub EvaluateMetaCommand(input As String)
    ForegroundColor = Red
    WriteLine($"Invalid command {input}.")
    ResetColor()
  End Sub

  Protected Sub ClearHistory()
    Me.m_submissionHistory.Clear()
  End Sub

  Protected Overridable Sub RenderLine(line As String)
    Write(line)
  End Sub

  Protected MustOverride Function IsCompleteSubmission(text As String) As Boolean

  Protected MustOverride Sub EvaluateSubmission(text As String)

End Class