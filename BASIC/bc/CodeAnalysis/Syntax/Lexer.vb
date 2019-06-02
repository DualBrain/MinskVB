Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Friend NotInheritable Class Lexer

    Private ReadOnly Property Text As String
    Private Property Position As Integer
    Private m_diagnostics As List(Of String) = New List(Of String)

    Public Sub New(text As String)
      Me.Text = text
    End Sub

    Public ReadOnly Property Diagnostics As IEnumerable(Of String)
      Get
        Return Me.m_diagnostics
      End Get
    End Property

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
          Return Chr(0)
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
        Return New SyntaxToken(SyntaxKind.EndOfFileToken, Me.PositionPlusPlus, Chr(0), Nothing)
      End If

      If Char.IsDigit(Me.Current) Then

        Dim start = Me.Position

        While Char.IsDigit(Me.Current)
          Me.Next()
        End While

        Dim length = Me.Position - start
        Dim text = Me.Text.Substring(start, length)

        Dim value As Integer
        If Not Integer.TryParse(text, value) Then
          Me.m_diagnostics.Add($"The number {Me.Text} isn't a valid Integer")
        End If
        Return New SyntaxToken(SyntaxKind.NumberToken, start, text, value)

      End If

      If Char.IsWhiteSpace(Me.Current) Then

        Dim start = Me.Position

        While Char.IsWhiteSpace(Me.Current)
          Me.Next()
        End While

        Dim length = Me.Position - start
        Dim text = Me.Text.Substring(start, length)

        Return New SyntaxToken(SyntaxKind.WhitespaceToken, start, text, Nothing)

      End If

      If Char.IsLetter(Me.Current) Then

        Dim start = Me.Position

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

        Case "!"c : Return New SyntaxToken(SyntaxKind.BangToken, Me.PositionPlusPlus, "!", Nothing)
        Case "&"c
          If Me.LookAhead = "&"c Then
            Dim position = Me.Position
            Me.Position += 2
            Return New SyntaxToken(SyntaxKind.AmpersandAmpersandToken, position, "&&", Nothing)
          End If
        Case "|"c
          If Me.LookAhead = "|"c Then
            Dim position = Me.Position
            Me.Position += 2
            Return New SyntaxToken(SyntaxKind.PipePipeToken, position, "||", Nothing)
          End If

        Case Else
      End Select

      Me.m_diagnostics.Add($"ERROR: Bad character input: '{Me.Current}'")
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