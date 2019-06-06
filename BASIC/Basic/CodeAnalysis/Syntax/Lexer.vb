Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.CodeAnalysis.Syntax

  Friend NotInheritable Class Lexer

    Public ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag

    Private ReadOnly Property Text As SourceText

    Private Property Position As Integer
    Private Property Start As Integer
    Private Property Kind As SyntaxKind
    Private Property Value As Object

    Public Sub New(text As SourceText)
      Me.Text = text
    End Sub

    Private ReadOnly Property Current As Char
      Get
        Return Me.Peek(0)
      End Get
    End Property

    Private ReadOnly Property LookAhead As Char
      Get
        Return Me.Peek(1)
      End Get
    End Property

    Private ReadOnly Property Peek(offset As Integer) As Char
      Get
        Dim index = Me.Position + offset
        If index >= Me.Text.Length Then
          Return ChrW(0)
        End If
        Return Me.Text(index)
      End Get
    End Property

    Public Function Lex() As SyntaxToken

      Me.Start = Me.Position
      Me.Kind = SyntaxKind.BadToken
      Me.Value = Nothing

      Select Case Me.Current
        Case ChrW(0) : Me.Kind = SyntaxKind.EndOfFileToken ': Me.Position += 1
        Case "+"c : Me.Kind = SyntaxKind.PlusToken : Me.Position += 1
        Case "-"c : Me.Kind = SyntaxKind.MinusToken : Me.Position += 1
        Case "*"c : Me.Kind = SyntaxKind.StarToken : Me.Position += 1
        Case "/"c : Me.Kind = SyntaxKind.SlashToken : Me.Position += 1
        Case "("c : Me.Kind = SyntaxKind.OpenParenToken : Me.Position += 1
        Case ")"c : Me.Kind = SyntaxKind.CloseParenToken : Me.Position += 1
        Case "&"c
          If Me.LookAhead = "&"c Then
            Me.Kind = SyntaxKind.AmpersandAmpersandToken : Me.Position += 2
          End If
        Case "|"c
          If Me.LookAhead = "|"c Then
            Me.Kind = SyntaxKind.PipePipeToken : Me.Position += 2
          End If
        Case "="c
          If Me.LookAhead = "="c Then
            Me.Kind = SyntaxKind.EqualsEqualsToken : Me.Position += 2
          Else
            Me.Kind = SyntaxKind.EqualsToken : Me.Position += 1
          End If
        Case "!"c
          If Me.LookAhead = "="c Then
            Me.Kind = SyntaxKind.BangEqualsToken : Me.Position += 2
          Else
            Me.Kind = SyntaxKind.BangToken : Me.Position += 1
          End If
        Case "<"c
          If Me.LookAhead = ">"c Then
            Me.Kind = SyntaxKind.LessThanGreaterThanToken : Me.Position += 2
          End If
        Case "0"c, "1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c
          Me.ReadNumberToken()
        Case " "c, ChrW(10), ChrW(13), ChrW(9) ' Short-circuit whitespace checking (common).
          Me.ReadWhiteSpace()
        Case Else
          If Char.IsLetter(Me.Current) Then
            Me.ReadIdentifierOrKeyword()
          ElseIf Char.IsWhiteSpace(Me.Current) Then
            Me.ReadWhiteSpace()
          Else
            Me.Diagnostics.ReportBadCharacter(Me.Position, Me.Current)
            Me.Position += 1
          End If

      End Select

      Dim length = Me.Position - Me.Start
      Dim text = SyntaxFacts.GetText(Me.Kind)
      If text Is Nothing Then
        text = Me.Text.ToString(Me.Start, length)
      End If

      Return New SyntaxToken(Me.Kind, Me.Start, text, Me.Value)

    End Function

    Private Sub ReadNumberToken()

      While Char.IsDigit(Me.Current)
        Me.Position += 1
      End While

      Dim length = Me.Position - Me.Start
      Dim text = Me.Text.ToString(Me.Start, length)
      Dim value As Integer
      If Not Integer.TryParse(text, value) Then
        Me.Diagnostics.ReportInvalidNumber(New TextSpan(Me.Start, length), text, GetType(Integer))
      End If

      Me.Value = value
      Me.Kind = SyntaxKind.NumberToken

    End Sub

    Private Sub ReadWhiteSpace()

      While Char.IsWhiteSpace(Me.Current)
        Me.Position += 1
      End While

      Me.Kind = SyntaxKind.WhitespaceToken

    End Sub

    Private Sub ReadIdentifierOrKeyword()

      While Char.IsLetter(Me.Current)
        Me.Position += 1
      End While

      Dim length = Me.Position - Me.Start
      Dim text = Me.Text.ToString(Me.Start, length)

      Me.Kind = SyntaxFacts.GetKeywordKind(text)

    End Sub

    'Private Function PositionPlusPlus() As Integer

    '  ' This will return the current Position as the "current" value and
    '  ' increment the Position for use the next time around...
    '  ' Effectively working similar to the C style post ++ addition:
    '  ' ..., position++, 

    '  Dim result = Me.Position
    '  Me.Position += 1
    '  Return result

    'End Function

  End Class

End Namespace