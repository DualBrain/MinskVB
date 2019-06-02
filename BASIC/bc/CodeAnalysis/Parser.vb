Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis

  Friend NotInheritable Class Parser

    Private ReadOnly Property Tokens As SyntaxToken()
    Private Property Position As Integer

    Private m_diagnostics As List(Of String) = New List(Of String)

    Sub New(text As String)
      Dim tokens = New List(Of SyntaxToken)
      Dim lexer = New Lexer(text)
      Dim token As SyntaxToken
      Do
        token = lexer.Lex
        If token.Kind <> SyntaxKind.WhitespaceToken AndAlso
           token.Kind <> SyntaxKind.BadToken Then
          tokens.Add(token)
        End If
      Loop While token.Kind <> SyntaxKind.EndOfFileToken
      Me.Tokens = tokens.ToArray
      Me.m_diagnostics.AddRange(lexer.Diagnostics)
    End Sub

    Public ReadOnly Property Diagnostics As IEnumerable(Of String)
      Get
        Return Me.m_diagnostics
      End Get
    End Property

    Private Function Peek(offset As Integer) As SyntaxToken
      Dim index = Me.Position + offset
      If index >= Me.Tokens.Length Then
        Return Me.Tokens(Me.Tokens.Length - 1)
      End If
      Return Me.Tokens(index)
    End Function

    Private Function Current() As SyntaxToken
      Return Me.Peek(0)
    End Function

    Private Function NextToken() As SyntaxToken
      Dim current = Me.Current
      Me.Position += 1
      Return current
    End Function

    Private Function MatchToken(kind As SyntaxKind) As SyntaxToken
      If Me.Current.Kind = kind Then
        Return Me.NextToken()
      Else
        Me.m_diagnostics.Add($"ERROR: Unexpected token <{Me.Current.Kind}>, expected <{kind}>")
        Return New SyntaxToken(kind, Me.Current.Position, Nothing, Nothing)
      End If
    End Function

    Public Function Parse() As SyntaxTree
      Dim expression = Me.ParseExpression
      Dim endOfFileToken = Me.MatchToken(SyntaxKind.EndOfFileToken)
      Return New SyntaxTree(Me.m_diagnostics, expression, endOfFileToken)
    End Function

    'Private Function ParseExpression() As ExpressionSyntax
    '  Return Me.ParseTerm()
    'End Function

    Private Function ParseExpression(Optional parentPrecedence As Integer = 0) As ExpressionSyntax

      Dim left = Me.ParsePrimaryExpression

      While True

        Dim precedence = GetBinaryOperatorPrecedence(Me.Current.Kind)
        If precedence = 0 OrElse precedence <= parentPrecedence Then
          Exit While
        End If
        Dim operatorToken = Me.NextToken()
        Dim right = Me.ParseExpression(precedence)
        left = New BinaryExpressionSyntax(left, operatorToken, right)

      End While

      Return left

    End Function

    Private Shared Function GetBinaryOperatorPrecedence(kind As SyntaxKind) As Integer

      Select Case kind
        Case SyntaxKind.StarToken, SyntaxKind.SlashToken
          Return 2
        Case SyntaxKind.PlusToken, SyntaxKind.MinusToken
          Return 1
        Case Else
          Return 0
      End Select

    End Function

    'Private Function ParseTerm() As ExpressionSyntax

    '  Dim left = Me.ParseFactor

    '  While Me.Current.Kind = SyntaxKind.PlusToken OrElse
    '        Me.Current.Kind = SyntaxKind.MinusToken

    '    Dim operatorToken = Me.NextToken()
    '    Dim right = Me.ParsePrimaryExpression()
    '    left = New BinaryExpressionSyntax(left, operatorToken, right)

    '  End While

    '  Return left

    'End Function

    'Private Function ParseFactor() As ExpressionSyntax

    '  Dim left = Me.ParsePrimaryExpression

    '  While Me.Current.Kind = SyntaxKind.StarToken OrElse
    '        Me.Current.Kind = SyntaxKind.SlashToken

    '    Dim operatorToken = Me.NextToken()
    '    Dim right = Me.ParsePrimaryExpression()
    '    left = New BinaryExpressionSyntax(left, operatorToken, right)

    '  End While

    '  Return left

    'End Function

    Private Function ParsePrimaryExpression() As ExpressionSyntax

      If Me.Current.Kind = SyntaxKind.OpenParenToken Then
        Dim left = Me.NextToken
        Dim expression = Me.ParseExpression
        Dim right = Me.MatchToken(SyntaxKind.CloseParenToken)
        Return New ParenExpressionSyntax(left, expression, right)
      End If

      Dim numberToken = MatchToken(SyntaxKind.NumberToken)
      Return New LiteralExpressionSyntax(numberToken)
    End Function

  End Class

End Namespace