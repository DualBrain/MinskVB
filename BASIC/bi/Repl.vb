﻿Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.ObjectModel
Imports System.Collections.Specialized
Imports System.ComponentModel
Imports System.Console
Imports System.ConsoleColor
Imports System.Reflection
Imports System.Security.Cryptography
Imports System.Text
Imports Basic.IO

Friend MustInherit Class Repl

  Private ReadOnly m_metaCommands As New List(Of MetaCommand)
  Private ReadOnly m_submissionHistory As New List(Of String)
  Private m_submissionHistoryIndex As Integer
  Private m_done As Boolean = False

  Friend Sub New()
    InitializeMetaCommands()
  End Sub

  Private Sub InitializeMetaCommands()

    Dim methods = [GetType]().GetMethods(BindingFlags.Public Or
                                         BindingFlags.NonPublic Or
                                         BindingFlags.Static Or
                                         BindingFlags.Instance Or
                                         BindingFlags.FlattenHierarchy)

    For Each method In methods

      'Dim attribute = CType(method.GetCustomAttribute(GetType(MetaCommandAttribute)), MetaCommandAttribute)
      Dim attribute = method.GetCustomAttribute(Of MetaCommandAttribute)

      If attribute Is Nothing Then Continue For

      Dim metaCommand = New MetaCommand(attribute.Name, attribute.Description, method)
      m_metaCommands.Add(metaCommand)

    Next

  End Sub

  Public Sub Run()
    Do
      Dim text = EditSubmission()
      If String.IsNullOrEmpty(text) Then
        'Return
      End If
      If Not text.Contains(Environment.NewLine) AndAlso text.StartsWith("#") Then
        EvaluateMetaCommand(text)
      Else
        EvaluateSubmission(text)
      End If
      m_submissionHistory.Add(text)
      m_submissionHistoryIndex = 0
    Loop
  End Sub

  Private Delegate Function LineRenderHandler(lines As IReadOnlyList(Of String), lineIndex As Integer, state As Object) As Object

  Private NotInheritable Class SubmissionView

    Private WithEvents SubmissionDocument As ObservableCollection(Of String)
    Private ReadOnly m_cursorTop As Integer
    Private ReadOnly m_lineRenderer As LineRenderHandler
    Private m_renderedLineCount As Integer
    Private m_currentLine As Integer
    Private m_currentCharacter As Integer

    Sub New(lineRenderer As LineRenderHandler, submissionDocument As ObservableCollection(Of String))
      m_lineRenderer = lineRenderer
      Me.SubmissionDocument = submissionDocument
      m_cursorTop = CursorTop
      Render()
    End Sub

    Private Sub CollectionChanged(sender As Object, e As NotifyCollectionChangedEventArgs) Handles SubmissionDocument.CollectionChanged
      Render()
    End Sub

    Private Sub Render()

      'SetCursorPosition(0, Me.m_cursorTop)
      CursorVisible = False

      Dim lineCount = 0
      Dim state = CObj(Nothing)

      For Each line In SubmissionDocument

        SetCursorPosition(0, m_cursorTop + lineCount)
        ForegroundColor = Green

        If lineCount = 0 Then
          Write("» ")
        Else
          Write("· ")
        End If

        ResetColor()
        m_lineRenderer(SubmissionDocument, lineCount, state)
        Write(New String(" "c, WindowWidth - line.Length - 2))
        lineCount += 1

      Next

      Dim numberOfBlankLines = m_renderedLineCount - lineCount
      If numberOfBlankLines > 0 Then
        Dim blankLine = New String(" "c, WindowWidth)
        For i = 0 To numberOfBlankLines - 1
          SetCursorPosition(0, m_cursorTop + lineCount + i)
          WriteLine(blankLine)
          'numberOfBlankLines -= 1
        Next
      End If

      m_renderedLineCount = lineCount
      CursorVisible = True
      UpdateCursorPosition()

    End Sub

    Private Sub UpdateCursorPosition()
      SetCursorPosition(2 + CurrentCharacter, m_cursorTop + CurrentLine)
    End Sub

    Public Property CurrentLine As Integer
      Get
        Return m_currentLine
      End Get
      Set
        If m_currentLine <> Value Then
          m_currentLine = Value
          m_currentCharacter = Math.Min(SubmissionDocument(m_currentLine).Length, m_currentCharacter)
          UpdateCursorPosition()
        End If
      End Set
    End Property

    Public Property CurrentCharacter As Integer
      Get
        Return m_currentCharacter
      End Get
      Set
        If m_currentCharacter <> Value Then
          m_currentCharacter = Value
          UpdateCursorPosition()
        End If
      End Set
    End Property

  End Class

  Private Function EditSubmission() As String

    m_done = False

    Dim document = New ObservableCollection(Of String) From {""}
    Dim view = New SubmissionView(AddressOf RenderLine, document)

    While Not m_done
      Dim key = ReadKey(True)
      HandleKey(key, document, view)
    End While

    view.CurrentLine = document.Count - 1
    view.CurrentCharacter = document(view.CurrentLine).Length
    WriteLine()

    Return String.Join(Environment.NewLine, document)

  End Function

  Private Sub HandleKey(key As ConsoleKeyInfo, document As ObservableCollection(Of String), view As SubmissionView)
    If key.Modifiers = 0 Then
      Select Case key.Key
        Case ConsoleKey.Escape : HandleEscape(document, view)
        Case ConsoleKey.Enter : HandleEnter(document, view)
        Case ConsoleKey.LeftArrow : HandleLeftArrow(document, view)
        Case ConsoleKey.RightArrow : HandleRightArrow(document, view)
        Case ConsoleKey.UpArrow : HandleUpArrow(document, view)
        Case ConsoleKey.DownArrow : HandleDownArrow(document, view)
        Case ConsoleKey.Backspace : HandleBackspace(document, view)
        Case ConsoleKey.Delete : HandleDelete(document, view)
        Case ConsoleKey.Home : HandleHome(document, view)
        Case ConsoleKey.End : HandleEnd(document, view)
        Case ConsoleKey.Tab : HandleTab(document, view)
        Case ConsoleKey.PageUp : HandlePageUp(document, view)
        Case ConsoleKey.PageDown : HandlePageDown(document, view)
        Case Else
      End Select
    ElseIf key.Modifiers = ConsoleModifiers.Control Then
      Select Case key.Key
        Case ConsoleKey.Enter
          HandleControlEnter(document, view)
        Case Else
      End Select
    End If
    'If key.KeyChar >= " "c Then
    If key.Key <> ConsoleKey.Backspace AndAlso key.KeyChar >= " "c Then
      HandleTyping(document, view, key.KeyChar.ToString)
    End If
  End Sub

  Private Shared Sub HandleEscape(document As ObservableCollection(Of String), view As SubmissionView)
    document.Clear()
    document.Add(String.Empty)
    view.CurrentLine = 0
    view.CurrentCharacter = 0
  End Sub

  Private Sub HandleEnter(document As ObservableCollection(Of String), view As SubmissionView)
    Dim submissionText = String.Join(Environment.NewLine, document)
    If submissionText.StartsWith("#") OrElse IsCompleteSubmission(submissionText) Then
      m_done = True
      Return
    End If
    InsertLine(document, view)
  End Sub

  Private Shared Sub HandleControlEnter(document As ObservableCollection(Of String), view As SubmissionView)
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

  Private Shared Sub HandleLeftArrow(document As ObservableCollection(Of String), view As SubmissionView)
    If document Is Nothing Then
    End If
    If view.CurrentCharacter > 0 Then
      view.CurrentCharacter -= 1
    End If
  End Sub

  Private Shared Sub HandleRightArrow(document As ObservableCollection(Of String), view As SubmissionView)
    Dim line = document(view.CurrentLine)
    If view.CurrentCharacter <= line.Length - 1 Then
      view.CurrentCharacter += 1
    End If
  End Sub

  Private Shared Sub HandleUpArrow(document As ObservableCollection(Of String), view As SubmissionView)
    If document Is Nothing Then
    End If
    If view.CurrentLine > 0 Then
      view.CurrentLine -= 1
    End If
  End Sub

  Private Shared Sub HandleDownArrow(document As ObservableCollection(Of String), view As SubmissionView)
    If view.CurrentLine < document.Count - 1 Then
      view.CurrentLine += 1
    End If
  End Sub

  Private Shared Sub HandleBackspace(document As ObservableCollection(Of String), view As SubmissionView)
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

  Private Shared Sub HandleDelete(document As ObservableCollection(Of String), view As SubmissionView)
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

  Private Shared Sub HandleHome(document As ObservableCollection(Of String), view As SubmissionView)
    If document Is Nothing Then
    End If
    view.CurrentCharacter = 0
  End Sub

  Private Shared Sub HandleEnd(document As ObservableCollection(Of String), view As SubmissionView)
    view.CurrentCharacter = document(view.CurrentLine).Length
  End Sub

  Private Sub HandlePageUp(document As ObservableCollection(Of String), view As SubmissionView)
    m_submissionHistoryIndex -= 1
    If m_submissionHistoryIndex < 0 Then
      m_submissionHistoryIndex = m_submissionHistory.Count - 1
    End If
    UpdateDocumentFromHistory(document, view)
  End Sub

  Private Sub HandlePageDown(document As ObservableCollection(Of String), view As SubmissionView)
    If m_submissionHistory.Count = 0 Then Return
    m_submissionHistoryIndex += 1
    If m_submissionHistoryIndex > m_submissionHistory.Count - 1 Then
      m_submissionHistoryIndex = 0
    End If
    UpdateDocumentFromHistory(document, view)
  End Sub

  Private Sub UpdateDocumentFromHistory(document As ObservableCollection(Of String), view As SubmissionView)
    If m_submissionHistory.Count = 0 Then Return
    document.Clear()
    Dim historyItem = m_submissionHistory(m_submissionHistoryIndex)
    Dim lines = historyItem.Split(Environment.NewLine)
    For Each line In lines
      document.Add(line)
    Next
    view.CurrentLine = document.Count - 1
    view.CurrentCharacter = document(view.CurrentLine).Length
  End Sub

  Private Shared Sub HandleTab(document As ObservableCollection(Of String), view As SubmissionView)
    Const TAB_WIDTH As Integer = 2
    Dim start = view.CurrentCharacter
    Dim remainingSpaces = TAB_WIDTH - start Mod TAB_WIDTH
    Dim line = document(view.CurrentLine)
    document(view.CurrentLine) = line.Insert(start, New String(" "c, remainingSpaces))
    view.CurrentCharacter += remainingSpaces
  End Sub

  Private Shared Sub HandleTyping(document As ObservableCollection(Of String), view As SubmissionView, text As String)
    Dim lineIndex = view.CurrentLine
    Dim start = view.CurrentCharacter
    document(lineIndex) = document(lineIndex).Insert(start, text)
    view.CurrentCharacter += text.Length
  End Sub

  Protected Sub EvaluateMetaCommand(input As String)

    ' Parse arguments

    Dim args = New List(Of String)
    Dim inQuotes = False
    Dim position = 1
    Dim sb = New StringBuilder
    While position < input.Length

      Dim c = input(position)
      Dim l = If(position + 1 >= input.Length, Chr(0), input(position + 1))

      If Char.IsWhiteSpace(c) Then
        If Not inQuotes Then
          CommitPendingArgument(args, sb)
        Else
          sb.Append(c)
        End If
      ElseIf c = Chr(34) Then
        If Not inQuotes Then
          inQuotes = True
        ElseIf l = Chr(34) Then
          sb.Append(c)
          position += 1
        Else
          inQuotes = False
        End If
      Else
        sb.Append(c)
      End If

      position += 1

    End While

    CommitPendingArgument(args, sb)

    Dim commandName = args.FirstOrDefault

    If args.Count > 0 Then
      args.RemoveAt(0)
    End If

    Dim command = m_metaCommands.SingleOrDefault(Function(mc) mc.Name = commandName)

    If command Is Nothing Then
      ForegroundColor = Red
      WriteLine($"Invalid command {input}.")
      ResetColor()
      Return
    End If

    Dim parameters = command.Method.GetParameters

    If args.Count <> parameters.Length Then
      Dim parameterNames = String.Join(" ", parameters.Select(Function(p) $"<{p.Name}>"))
      ForegroundColor = Red
      WriteLine($"error: invalid number of arguments")
      WriteLine($"usage: #{command.Name} {parameterNames}")
      ResetColor()
      Return
    End If

    'command.Method.Invoke(Me, args.ToArray)
    Dim instance = If(command.Method.IsStatic, Nothing, Me)
    command.Method.Invoke(instance, args.ToArray)

  End Sub

  Private Shared Sub CommitPendingArgument(args As List(Of String), sb As StringBuilder)

    Dim arg = sb.ToString

    If Not String.IsNullOrWhiteSpace(arg) Then
      args.Add(arg)
    End If

    sb.Clear()

  End Sub

  Protected Sub ClearHistory()
    m_submissionHistory.Clear()
  End Sub

  Protected Overridable Function RenderLine(lines As IReadOnlyList(Of String), lineIndex As Integer, state As Object) As Object
    Write(lines(lineIndex))
    Return state
  End Function

  Protected MustOverride Function IsCompleteSubmission(text As String) As Boolean

  Protected MustOverride Sub EvaluateSubmission(text As String)

  <AttributeUsage(AttributeTargets.Method, AllowMultiple:=False)>
  Protected NotInheritable Class MetaCommandAttribute
    Inherits Attribute

    Sub New(name As String, description As String)
      Me.Name = name
      Me.Description = description
    End Sub

    Public ReadOnly Property Name As String
    Public ReadOnly Property Description As String

  End Class

  Private Class MetaCommand

    Public Sub New(name As String, description As String, method As MethodInfo)
      Me.Name = name
      Me.Description = description
      Me.Method = method
    End Sub

    Public ReadOnly Property Name As String
    Public ReadOnly Property Description As String
    Public ReadOnly Property Method As MethodInfo

  End Class

  <MetaCommand("help", "Shows help")>
  Protected Sub EvaluateHelp()

    Dim maxNameLength = m_metaCommands.Max(Function(mc) mc.Name.Length)

    For Each metaCommand In m_metaCommands.OrderBy(Function(mc) mc.Name)
      'Dim paddedName = metaCommand.Name.PadRight(maxNameLength)

      Dim metaParams = metaCommand.Method.GetParameters
      If metaParams.Length = 0 Then
        Dim paddedName = metaCommand.Name.PadRight(maxNameLength)
        Console.Out.WritePunctuation("#")
        Console.Out.WriteIdentifier(paddedName)
      Else
        Console.Out.WritePunctuation("#")
        Console.Out.WriteIdentifier(metaCommand.Name)
        For Each pi In metaParams
          Console.Out.WriteSpace()
          Console.Out.WritePunctuation("<")
          Console.Out.WriteIdentifier(pi.Name)
          Console.Out.WritePunctuation(">")
        Next
        Console.Out.WriteLine()
        Console.Out.WriteSpace
        Console.Out.Write(Space(maxNameLength))
      End If

      'Console.Out.WritePunctuation("#")
      'Console.Out.WriteIdentifier(paddedName)
      Console.Out.WriteSpace()
      Console.Out.WriteSpace()
      Console.Out.WriteSpace()
      Console.Out.WritePunctuation(metaCommand.Description)
      Console.Out.WriteLine()

    Next

  End Sub

End Class