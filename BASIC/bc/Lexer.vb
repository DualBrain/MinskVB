Option Explicit On ' Variables must be "declared".
Option Strict On ' They must have a "type".
Option Infer On ' Where we can, the "type" can be inferred.

' This is a .NET Core 3.0 VB.NET project.
' I added the above Option's to enable how I like
' to write software.

' You can find the code at 
' https://github.com/dualbrain/basic
' Feel free to join in and contribute!

Class Lexer

  Private ReadOnly Property Text As String
  Private Property Position As Integer

  Public Sub New(text As String)
    Me.Text = text
  End Sub

  Private ReadOnly Property Current As Char
    Get
      If Me.Position >= Me.Text.Length Then
        Return Chr(0)
      End If
      Return Me.Text(Me.Position)
    End Get
  End Property

  Private Sub [Next]()
    Me.Position += 1
  End Sub

  Public Function NextToken() As SyntaxToken

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
      Integer.TryParse(text, value)
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

    Select Case Me.Current
      Case "+"c : Return New SyntaxToken(SyntaxKind.PlusToken, Me.PositionPlusPlus, "+", Nothing)
      Case "-"c : Return New SyntaxToken(SyntaxKind.MinusToken, Me.PositionPlusPlus, "-", Nothing)
      Case "*"c : Return New SyntaxToken(SyntaxKind.StarToken, Me.PositionPlusPlus, "*", Nothing)
      Case "/"c : Return New SyntaxToken(SyntaxKind.SlashToken, Me.PositionPlusPlus, "/", Nothing)
      Case "("c : Return New SyntaxToken(SyntaxKind.ParenOpenToken, Me.PositionPlusPlus, "(", Nothing)
      Case ")"c : Return New SyntaxToken(SyntaxKind.ParenCloseToken, Me.PositionPlusPlus, ")", Nothing)
      Case Else
    End Select

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