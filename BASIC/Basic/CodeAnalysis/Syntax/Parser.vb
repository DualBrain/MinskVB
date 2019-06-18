Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.CodeAnalysis.Syntax

  Friend NotInheritable Class Parser

    Public ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag
    Public ReadOnly Property Text As SourceText
    Private ReadOnly Property Tokens As ImmutableArray(Of SyntaxToken)

    Private Property Position As Integer

    Sub New(text As SourceText)
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
      Me.Text = text
      Me.Diagnostics.AddRange(lexer.Diagnostics)
      Me.Tokens = tokens.ToImmutableArray
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

    Public Function ParseCompilationUnit() As CompilationUnitSyntax
      Dim statement = Me.ParseStatement
      Dim endOfFileToken = Me.MatchToken(SyntaxKind.EndOfFileToken)
      Return New CompilationUnitSyntax(statement, endOfFileToken)
    End Function

    Private Function ParseStatement() As StatementSyntax
      Select Case Me.Current.Kind
        Case SyntaxKind.OpenBraceToken
          Return Me.ParseBlockStatement
        Case SyntaxKind.LetKeyword,
             SyntaxKind.VarKeyword,
             SyntaxKind.LetKeyword
          Return Me.ParseVariableDeclaration
        Case SyntaxKind.IfKeyword
          Return Me.ParseIfStatement
        Case SyntaxKind.WhileKeyword
          Return Me.ParseWhileStatement
        Case SyntaxKind.ForKeyword
          Return Me.ParseForStatement
        Case Else
          Return Me.ParseExpressionStatement
      End Select
    End Function

    Private Function ParseBlockStatement() As StatementSyntax

      Dim statements = ImmutableArray.CreateBuilder(Of StatementSyntax)

      Dim openBraceToken = Me.MatchToken(SyntaxKind.OpenBraceToken)

      While Me.Current.Kind <> SyntaxKind.EndOfFileToken AndAlso
            Me.Current.Kind <> SyntaxKind.CloseBraceToken

        Dim startToken = Me.Current

        Dim statement = Me.ParseStatement()
        statements.Add(statement)

        ' If ParseStatement() did not consume any tokens,
        ' we need to skip the current token and continue
        ' in order to avoid an infinite loop.
        ' We don't need to report an error because we'll
        ' already tried to parse an expression statement
        ' and reported one.
        If (Me.Current Is startToken) Then
          Me.NextToken()
        End If

      End While
      Dim closeBraceToken = Me.MatchToken(SyntaxKind.CloseBraceToken)

      Return New BlockStatementSyntax(openBraceToken, statements.ToImmutable, closeBraceToken)

    End Function

    Private Function ParseVariableDeclaration() As StatementSyntax

      ' The following line is modified from the original in order to
      ' allow the addition of the DIM keyword (in addition to LET and VAR).
      'Dim expected = If(Me.Current.Kind = SyntaxKind.LetKeyword, SyntaxKind.LetKeyword, SyntaxKind.VarKeyword)
      Dim expected = SyntaxKind.VarKeyword
      ' If LET or DIM, set... otherwise, default to VAR (whether it's VAR or not).
      Select Case Me.Current.Kind
        Case SyntaxKind.LetKeyword : expected = SyntaxKind.LetKeyword
        Case SyntaxKind.DimKeyword : expected = SyntaxKind.DimKeyword
        Case Else
      End Select

      Dim keyword = Me.MatchToken(expected)
      Dim identifier = Me.MatchToken(SyntaxKind.IdentifierToken)
      Dim equals = Me.MatchToken(SyntaxKind.EqualsToken)
      Dim initializer = Me.ParseExpression()

      Return New VariableDeclarationSyntax(keyword, identifier, equals, initializer)

    End Function

    Private Function ParseIfStatement() As StatementSyntax
      Dim keyword = Me.MatchToken(SyntaxKind.IfKeyword)
      Dim condition = Me.ParseExpression
      Dim statement = Me.ParseStatement()
      Dim elseClause = Me.ParseElseClause()
      Return New IfStatementSyntax(keyword, condition, statement, elseClause)
    End Function

    Private Function ParseElseClause() As ElseClauseSyntax
      If Me.Current.Kind <> SyntaxKind.ElseKeyword Then
        Return Nothing
      End If
      Dim keyword = Me.NextToken
      Dim statement = Me.ParseStatement
      Return New ElseClauseSyntax(keyword, statement)
    End Function

    Private Function ParseWhileStatement() As StatementSyntax
      Dim keyword = Me.MatchToken(SyntaxKind.WhileKeyword)
      Dim condition = Me.ParseExpression
      Dim body = Me.ParseStatement
      Return New WhileStatementSyntax(keyword, condition, body)
    End Function

    Private Function ParseForStatement() As StatementSyntax
      Dim keyword = Me.MatchToken(SyntaxKind.ForKeyword)
      Dim identifier = Me.MatchToken(SyntaxKind.IdentifierToken)
      Dim equalsToken = Me.MatchToken(SyntaxKind.EqualsToken)
      Dim lowerBound = Me.ParseExpression
      Dim toKeyword = Me.MatchToken(SyntaxKind.ToKeyword)
      Dim upperBound = Me.ParseExpression
      Dim body = Me.ParseStatement
      Return New ForStatementSyntax(keyword, identifier, equalsToken, lowerBound, toKeyword, upperBound, body)
    End Function

    Private Function ParseExpressionStatement() As ExpressionStatementSyntax
      Dim expression = Me.ParseExpression()
      Return New ExpressionStatementSyntax(expression)
    End Function

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

    Private Function ParsePrimaryExpression() As ExpressionSyntax

      Select Case Me.Current.Kind
        Case SyntaxKind.OpenParenToken : Return Me.ParseParenExpression
        Case SyntaxKind.FalseKeyword : Return Me.ParseBooleanLiteral
        Case SyntaxKind.TrueKeyword : Return Me.ParseBooleanLiteral
        Case SyntaxKind.NumberToken : Return Me.ParseNumberLiteral
        Case SyntaxKind.StringToken : Return Me.ParseStringLiteral
        Case SyntaxKind.IdentifierToken : Return Me.ParseNameorCallExpression
        Case Else
          ' Default to parsing a name expression if we reach this far.
          Return Me.ParseNameOrCallExpression
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

    Private Function ParseNumberLiteral() As ExpressionSyntax
      Dim numberToken = Me.MatchToken(SyntaxKind.NumberToken)
      Return New LiteralExpressionSyntax(numberToken)
    End Function

    Private Function ParseStringLiteral() As ExpressionSyntax
      Dim stringToken = Me.MatchToken(SyntaxKind.StringToken)
      Return New LiteralExpressionSyntax(stringToken)
    End Function

    Private Function ParseNameOrCallExpression() As ExpressionSyntax
      If Me.Peek(0).Kind = SyntaxKind.IdentifierToken AndAlso
         Me.Peek(1).Kind = SyntaxKind.OpenParenToken Then
        Return Me.ParseCallExpression
      Else
        Return Me.ParseNameExpression
      End If
    End Function

    Private Function ParseCallExpression() As ExpressionSyntax
      Dim identifier = Me.MatchToken(SyntaxKind.IdentifierToken)
      Dim openParen = Me.MatchToken(SyntaxKind.OpenParenToken)
      Dim arguments = Me.ParseArguments
      Dim closeParen = Me.MatchToken(SyntaxKind.CloseParenToken)
      Return New CallExpressionSyntax(identifier, openParen, arguments, closeParen)
    End Function

    Private Function ParseArguments() As SeparatedSyntaxList(Of ExpressionSyntax)

      Dim nodesAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxNode)

      While Me.Current.Kind <> SyntaxKind.CloseParenToken AndAlso
            Me.Current.Kind <> SyntaxKind.EndOfFileToken

        Dim expression = Me.ParseExpression
        nodesAndSeparators.Add(expression)

        If Me.Current.Kind <> SyntaxKind.CloseParenToken Then
          Dim comma = Me.MatchToken(SyntaxKind.CommaToken)
          nodesAndSeparators.Add(comma)
        End If

      End While

      Return New SeparatedSyntaxList(Of ExpressionSyntax)(nodesAndSeparators.ToImmutable)

    End Function

    Private Function ParseNameExpression() As ExpressionSyntax
      Dim identifierToken = Me.MatchToken(SyntaxKind.IdentifierToken)
      Return New NameExpressionSyntax(identifierToken)
    End Function

  End Class

End Namespace