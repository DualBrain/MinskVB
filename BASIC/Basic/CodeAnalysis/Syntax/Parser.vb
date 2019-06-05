Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable

Namespace Global.Basic.CodeAnalysis.Syntax

  Friend NotInheritable Class Parser

    Public ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag
    Private ReadOnly Property Tokens As ImmutableArray(Of SyntaxToken)

    Private Property Position As Integer

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
      Me.Tokens = tokens.ToImmutableArray
      Me.Diagnostics.AddRange(lexer.Diagnostics)
    End Sub

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
        Me.Diagnostics.ReportUnexpectedToken(Me.Current.Span, Me.Current.Kind, kind)
        Return New SyntaxToken(kind, Me.Current.Position, Nothing, Nothing)
      End If
    End Function

    Public Function Parse() As SyntaxTree
      Dim expression = Me.ParseExpression
      Dim endOfFileToken = Me.MatchToken(SyntaxKind.EndOfFileToken)
      Return New SyntaxTree(Me.Diagnostics.ToImmutableArray, expression, endOfFileToken)
    End Function

    'Private Function ParseExpression() As ExpressionSyntax
    '  Return Me.ParseTerm()
    'End Function

    Private Function ParseExpression() As ExpressionSyntax
      Return Me.ParseAssignmentExpression
    End Function

    Private Function ParseAssignmentExpression() As ExpressionSyntax

      If (Me.Peek(0).Kind = SyntaxKind.IdentifierToken AndAlso
                Me.Peek(1).Kind = SyntaxKind.EqualsToken) Then

        Dim identifierToken = Me.NextToken
        Dim operatorToken = Me.NextToken
        Dim right = Me.ParseAssignmentExpression
        Return New AssignmentExpressionSyntax(identifierToken, operatorToken, right)

      End If

      Return Me.ParseBinaryExpression

    End Function

    Private Function ParseBinaryExpression(Optional parentPrecedence As Integer = 0) As ExpressionSyntax

      Dim left As ExpressionSyntax
      Dim unaryOperatorPrecedence = Me.Current.Kind.GetUnaryOperatorPrecedence
      If unaryOperatorPrecedence <> 0 AndAlso unaryOperatorPrecedence >= parentPrecedence Then
        Dim operatorToken = Me.NextToken()
        Dim operand = Me.ParseBinaryExpression(unaryOperatorPrecedence)
        left = New UnaryExpressionSyntax(operatorToken, operand)
      Else
        left = Me.ParsePrimaryExpression
      End If

      While True

        Dim precedence = Me.Current.Kind.GetBinaryOperatorPrecedence
        If precedence = 0 OrElse precedence <= parentPrecedence Then
          Exit While
        End If
        Dim operatorToken = Me.NextToken()
        Dim right = Me.ParseBinaryExpression(precedence)
        left = New BinaryExpressionSyntax(left, operatorToken, right)

      End While

      Return left

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

      Select Case Me.Current.Kind
        Case SyntaxKind.OpenParenToken : Return Me.ParseParenExpression
        Case SyntaxKind.FalseKeyword : Return Me.ParseBooleanLiteral
        Case SyntaxKind.TrueKeyword : Return Me.ParseBooleanLiteral
        Case SyntaxKind.NumberToken : Return Me.ParseNumberLiteral
        Case SyntaxKind.IdentifierToken : Return Me.ParseNameExpression
        Case Else
          ' Default to parsing a name expression if we reach this far.
          Return Me.ParseNameExpression
      End Select

    End Function

    Private Function ParseParenExpression() As ExpressionSyntax
      Dim left = Me.MatchToken(SyntaxKind.OpenParenToken)
      Dim expression = Me.ParseExpression
      Dim right = Me.MatchToken(SyntaxKind.CloseParenToken)
      Return New ParenExpressionSyntax(left, expression, right)
    End Function

    Private Function ParseBooleanLiteral() As ExpressionSyntax
      Dim isTrue = (Me.Current.Kind = SyntaxKind.TrueKeyword)
      Dim keywordToken = Me.MatchToken(If(isTrue, SyntaxKind.TrueKeyword, SyntaxKind.FalseKeyword))
      Return New LiteralExpressionSyntax(keywordToken, isTrue)
    End Function

    Private Function ParseNameExpression() As ExpressionSyntax
      Dim identifierToken = Me.MatchToken(SyntaxKind.IdentifierToken)
      Return New NameExpressionSyntax(identifierToken)
    End Function

    Private Function ParseNumberLiteral() As ExpressionSyntax
      Dim numberToken = Me.MatchToken(SyntaxKind.NumberToken)
      Return New LiteralExpressionSyntax(numberToken)
    End Function

  End Class

End Namespace