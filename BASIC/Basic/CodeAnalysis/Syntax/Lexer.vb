Option Explicit On
Option Strict On
Option Infer On

Imports System.Text
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.CodeAnalysis.Syntax

  Friend NotInheritable Class Lexer

    Public ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag

    Private ReadOnly Property Text As SourceText
    Private ReadOnly m_syntaxTree As SyntaxTree

    Private Property Position As Integer
    Private Property Start As Integer
    Private Property Kind As SyntaxKind
    Private Property Value As Object

    Public Sub New(tree As SyntaxTree)
      m_syntaxTree = tree
      Text = tree.Text
    End Sub

    Private ReadOnly Property Current As Char
      Get
        Return Peek(0)
      End Get
    End Property

    Private ReadOnly Property LookAhead As Char
      Get
        Return Peek(1)
      End Get
    End Property

    Private ReadOnly Property Peek(offset As Integer) As Char
      Get
        Dim index = Position + offset
        If index >= Text.Length Then
          Return ChrW(0)
        End If
        Return Text(index)
      End Get
    End Property

    Public Function Lex() As SyntaxToken

      Start = Position
      Kind = SyntaxKind.BadTokenTrivia
      Value = Nothing

      Select Case Current
        Case ChrW(0) : Kind = SyntaxKind.EndOfFileToken ': Me.Position += 1
        Case "+"c : Kind = SyntaxKind.PlusToken : Position += 1
        Case "-"c : Kind = SyntaxKind.MinusToken : Position += 1
        Case "*"c : Kind = SyntaxKind.StarToken : Position += 1
        Case "/"c
          If LookAhead = "/" Then
            ReadSingleLineComment()
          ElseIf LookAhead = "*" Then
            ReadMultiLineComment()
          Else
            Kind = SyntaxKind.SlashToken : Position += 1
          End If
        Case "("c : Kind = SyntaxKind.OpenParenToken : Position += 1
        Case ")"c : Kind = SyntaxKind.CloseParenToken : Position += 1
        Case "{"c : Kind = SyntaxKind.OpenBraceToken : Position += 1
        Case "}"c : Kind = SyntaxKind.CloseBraceToken : Position += 1
        Case ":"c : Kind = SyntaxKind.ColonToken : Position += 1
        Case ","c : Kind = SyntaxKind.CommaToken : Position += 1
        Case "~"c : Kind = SyntaxKind.TildeToken : Position += 1
        Case "^"c : Kind = SyntaxKind.HatToken : Position += 1
        Case "&"c
          If LookAhead = "&"c Then
            Kind = SyntaxKind.AmpersandAmpersandToken : Position += 2
          Else
            Kind = SyntaxKind.AmpersandToken : Position += 1
          End If
        Case "|"c
          If LookAhead = "|"c Then
            Kind = SyntaxKind.PipePipeToken : Position += 2
          Else
            Kind = SyntaxKind.PipeToken : Position += 1
          End If
        Case "="c
          If LookAhead = "="c Then
            Kind = SyntaxKind.EqualsEqualsToken : Position += 2
          Else
            Kind = SyntaxKind.EqualsToken : Position += 1
          End If
        Case "!"c
          If LookAhead = "="c Then
            Kind = SyntaxKind.BangEqualsToken : Position += 2
          Else
            Kind = SyntaxKind.BangToken : Position += 1
          End If
        Case "<"c
          'If LookAhead = ">"c Then
          '  Kind = SyntaxKind.LessThanGreaterThanToken : Position += 2
          If LookAhead = "="c Then
            Kind = SyntaxKind.LessThanEqualsToken : Position += 2
          Else
            Kind = SyntaxKind.LessThanToken : Position += 1
          End If
        Case ">"c
          If LookAhead = "="c Then
            Kind = SyntaxKind.GreaterThanEqualsToken : Position += 2
          Else
            Kind = SyntaxKind.GreaterThanToken : Position += 1
          End If
        Case ChrW(34)
          ReadString()
        Case "0"c, "1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c
          ReadNumberToken()
        Case " "c, ChrW(10), ChrW(13), ChrW(9) ' Short-circuit whitespace checking (common).
          ReadWhiteSpace()
        Case Else
          If Char.IsLetter(Current) OrElse Current = "$"c Then
            ReadIdentifierOrKeyword()
          ElseIf Char.IsWhiteSpace(Current) Then
            ReadWhiteSpace()
          Else
            Dim span = New TextSpan(Position, 1)
            Dim location = New TextLocation(Me.Text, span)
            Diagnostics.ReportBadCharacter(location, Current)
            Position += 1
          End If

      End Select

      Dim length = Position - Start
      Dim text = SyntaxFacts.GetText(Kind)
      If text Is Nothing Then
        text = Me.Text.ToString(Start, length)
      End If

      Return New SyntaxToken(m_syntaxTree, Kind, Start, text, Value)

    End Function

    Private Sub ReadNumberToken()

      While Char.IsDigit(Current)
        Position += 1
      End While

      Dim length = Position - Start
      Dim text = Me.Text.ToString(Start, length)
      Dim value As Integer
      If Not Integer.TryParse(text, value) Then
        Dim location = New TextLocation(Me.Text, New TextSpan(Start, length))
        Diagnostics.ReportInvalidNumber(location, text, TypeSymbol.Int)
      End If

      Me.Value = value
      Kind = SyntaxKind.NumberToken

    End Sub

    Private Sub ReadSingleLineComment()

      _Position += 2

      Dim done = False
      While Not done
        Select Case Current
          Case ChrW(0), ChrW(13), ChrW(10)
            done = True
          Case Else
            _Position += 1
        End Select
      End While

      _Kind = SyntaxKind.SingleLineCommentTrivia

    End Sub

    Private Sub ReadMultiLineComment()

      _Position += 2

      Dim done = False
      While Not done
        Select Case Current
          Case ChrW(0)
            Dim span = New TextSpan(Start, 2)
            Dim location = New TextLocation(Text, span)
            Diagnostics.ReportUnterminatedMultiLineComment(location)
            done = True
          Case "*"c
            If LookAhead = "/" Then
              done = True
              _Position += 1
            End If
            _Position += 1
          Case Else
            _Position += 1
        End Select
      End While

      _Kind = SyntaxKind.MultiLineCommentTrivia

    End Sub

    Private Sub ReadString()

      ' "Test \" dddd"
      ' "Test "" dddd"

      ' skip the current quote
      Position += 1

      Dim sb = New StringBuilder
      Dim done = False

      While Not done
        Select Case Current
          Case ChrW(0), ChrW(13), ChrW(10)
            Dim span = New TextSpan(Start, 1)
            Dim location = New TextLocation(Me.Text, span)
            Diagnostics.ReportUnterminatedString(location)
            done = True
          Case """"c
            If LookAhead = """"c Then
              sb.Append(Current)
              Position += 2
            Else
              Position += 1
              done = True
            End If
          Case Else
            sb.Append(Current)
            Position += 1
        End Select
      End While

      Kind = SyntaxKind.StringToken
      Value = sb.ToString

    End Sub

    Private Sub ReadWhiteSpace()

      While Char.IsWhiteSpace(Current)
        Position += 1
      End While

      Kind = SyntaxKind.WhitespaceTrivia

    End Sub

    Private Sub ReadIdentifierOrKeyword()

      While Char.IsLetter(Current) OrElse Current = "$"c
        Position += 1
      End While

      Dim length = Position - Start
      Dim text = Me.Text.ToString(Start, length)

      Kind = SyntaxFacts.GetKeywordKind(text)

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