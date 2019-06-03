Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Friend NotInheritable Class Lexer

    Private ReadOnly Property Text As String
    Private Property Position As Integer

    Public Sub New(text As String)
      Me.Text = text
    End Sub

    Public ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag

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

    Private Sub [Next]()
      Me.Position += 1
    End Sub

    Public Function Lex() As SyntaxToken

      ' numbers
      ' symbols
      ' whitespace
      ' "end of file"

      If Me.Position >= Me.Text.Length Then
        Return New SyntaxToken(SyntaxKind.EndOfFileToken, Me.PositionPlusPlus, ChrW(0), Nothing)
      End If

      Dim start = Me.Position

      If Char.IsDigit(Me.Current) Then

        While Char.IsDigit(Me.Current)
          Me.Next()
        End While

        Dim length = Me.Position - start
        Dim text = Me.Text.Substring(start, length)

        Dim value As Integer
        If Not Integer.TryParse(text, value) Then
          Me.Diagnostics.ReportInvalidNumber(New TextSpan(start, length), Me.Text, GetType(Integer))
        End If
        Return New SyntaxToken(SyntaxKind.NumberToken, start, text, value)

      End If

      If Char.IsWhiteSpace(Me.Current) Then

        While Char.IsWhiteSpace(Me.Current)
          Me.Next()
        End While

        Dim length = Me.Position - start
        Dim text = Me.Text.Substring(start, length)

        Return New SyntaxToken(SyntaxKind.WhitespaceToken, start, text, Nothing)

      End If

      If Char.IsLetter(Me.Current) Then

        While Char.IsLetter(Me.Current)
          Me.Next()
        End While

        Dim length = Me.Position - start
        Dim text = Me.Text.Substring(start, length)
        Dim kind = SyntaxFacts.GetKeywordKind(text)

        Return New SyntaxToken(kind, start, text, Nothing)

      End If

      ' true
      ' false

      Select Case Me.Current
        Case "+"c : Return New SyntaxToken(SyntaxKind.PlusToken, Me.PositionPlusPlus, "+", Nothing)
        Case "-"c : Return New SyntaxToken(SyntaxKind.MinusToken, Me.PositionPlusPlus, "-", Nothing)
        Case "*"c : Return New SyntaxToken(SyntaxKind.StarToken, Me.PositionPlusPlus, "*", Nothing)
        Case "/"c : Return New SyntaxToken(SyntaxKind.SlashToken, Me.PositionPlusPlus, "/", Nothing)
        Case "("c : Return New SyntaxToken(SyntaxKind.OpenParenToken, Me.PositionPlusPlus, "(", Nothing)
        Case ")"c : Return New SyntaxToken(SyntaxKind.CloseParenToken, Me.PositionPlusPlus, ")", Nothing)

        'Case "!"c : Return New SyntaxToken(SyntaxKind.BangToken, Me.PositionPlusPlus, "!", Nothing)
        Case "&"c
          If Me.LookAhead = "&"c Then
            Me.Position += 2
            Return New SyntaxToken(SyntaxKind.AmpersandAmpersandToken, start, "&&", Nothing)
          End If
        Case "|"c
          If Me.LookAhead = "|"c Then
            Me.Position += 2
            Return New SyntaxToken(SyntaxKind.PipePipeToken, start, "||", Nothing)
          End If

        Case "="c
          If Me.LookAhead = "="c Then
            Me.Position += 2
            Return New SyntaxToken(SyntaxKind.EqualsEqualsToken, start, "==", Nothing)
          Else
            Return New SyntaxToken(SyntaxKind.EqualsToken, Me.PositionPlusPlus, "=", Nothing)
          End If
        Case "!"c
          If Me.LookAhead = "="c Then
            Me.Position += 2
            Return New SyntaxToken(SyntaxKind.BangEqualsToken, start, "!=", Nothing)
          Else
            Me.Position += 1
            Return New SyntaxToken(SyntaxKind.BangToken, start, "!", Nothing)
          End If
        Case "<"c
          If Me.LookAhead = ">"c Then
            Me.Position += 2
            Return New SyntaxToken(SyntaxKind.LessThanGreaterThanToken, start, "<>", Nothing)
          End If

        Case Else
      End Select

      Me.Diagnostics.ReportBadCharacter(Me.Position, Me.Current)
      Return New SyntaxToken(SyntaxKind.BadToken, Me.PositionPlusPlus, Me.Text.Substring(Me.Position - 1, 1), Nothing)

    End Function

    Private Function PositionPlusPlus() As Integer

      ' This will return the current Position as the "current" value and
      ' increment the Position for use the next time around...
      ' Effectively working similar to the C style post ++ addition:
      ' ..., position++, 

      Dim result = Me.Position
      Me.Position += 1
      Return result

    End Function

  End Class

End Namespace