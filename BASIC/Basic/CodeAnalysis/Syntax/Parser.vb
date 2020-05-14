Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.CodeAnalysis.Syntax

  Friend NotInheritable Class Parser

    Public ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag
    Private ReadOnly m_syntaxTree As SyntaxTree
    Public ReadOnly Property Text As SourceText
    Private ReadOnly Property Tokens As ImmutableArray(Of SyntaxToken)

    Private Property Position As Integer

    Sub New(tree As SyntaxTree)
      Dim tokens = New List(Of SyntaxToken)
      Dim lexer = New Lexer(tree)
      Dim token As SyntaxToken
      Do
        token = lexer.Lex
        If token.Kind <> SyntaxKind.WhitespaceToken AndAlso
           token.Kind <> SyntaxKind.SingleLineCommentToken AndAlso
           token.Kind <> SyntaxKind.MultiLineCommentToken AndAlso
           token.Kind <> SyntaxKind.BadToken Then
          tokens.Add(token)
        End If
      Loop While token.Kind <> SyntaxKind.EndOfFileToken
      m_syntaxTree = tree
      Text = tree.Text
      Diagnostics.AddRange(lexer.Diagnostics)
      Me.Tokens = tokens.ToImmutableArray
    End Sub

    Private Function Peek(offset As Integer) As SyntaxToken
      Dim index = Position + offset
      If index >= Tokens.Length Then
        Return Tokens(Tokens.Length - 1)
      End If
      Return Tokens(index)
    End Function

    Private Function Current() As SyntaxToken
      Return Peek(0)
    End Function

    Private Function NextToken() As SyntaxToken
      Dim current = Me.Current
      Position += 1
      Return current
    End Function

    Private Function MatchToken(kind As SyntaxKind) As SyntaxToken
      If Current.Kind = kind Then
        Return NextToken()
      Else
        Diagnostics.ReportUnexpectedToken(Current.Location, Current.Kind, kind)
        Return New SyntaxToken(m_syntaxTree, kind, Current.Position, Nothing, Nothing)
      End If
    End Function

    Public Function ParseCompilationUnit() As CompilationUnitSyntax
      Dim members = ParseMembers
      Dim endOfFileToken = MatchToken(SyntaxKind.EndOfFileToken)
      Return New CompilationUnitSyntax(m_syntaxTree, members, endOfFileToken)
    End Function

    Private Function ParseMembers() As ImmutableArray(Of MemberSyntax)

      Dim members = ImmutableArray.CreateBuilder(Of MemberSyntax)

      While Current.Kind <> SyntaxKind.EndOfFileToken

        Dim startToken = Current

        Dim member = ParseMember()
        members.Add(member)

        ' If ParseStatement() did not consume any tokens,
        ' we need to skip the current token and continue
        ' in order to avoid an infinite loop.
        ' We don't need to report an error because we'll
        ' already tried to parse an expression statement
        ' and reported one.
        If (Current Is startToken) Then
          NextToken()
        End If

      End While

      Return members.ToImmutable

    End Function

    Private Function ParseMember() As MemberSyntax
      If Me.Current.Kind = SyntaxKind.FunctionKeyword Then
        Return ParseFunctionDeclaration
      End If
      Return ParseGlobalStatement
    End Function

    Private Function ParseFunctionDeclaration() As MemberSyntax
      Dim functionKeyword = MatchToken(SyntaxKind.FunctionKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
      Dim parameters = ParseParameterList
      Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
      Dim type = ParseOptionalTypeClause
      Dim body = ParseBlockStatement()
      Return New FunctionDeclarationSyntax(m_syntaxTree, functionKeyword, identifier, openParen, parameters, closeParen, type, body)
    End Function

    Private Function ParseParameterList() As SeparatedSyntaxList(Of ParameterSyntax)

      Dim nodesAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxNode)

      Dim parseNextParameter = True
      While parseNextParameter AndAlso
            Current.Kind <> SyntaxKind.CloseParenToken AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken

        Dim parameter = ParseParameter
        nodesAndSeparators.Add(parameter)

        If Me.Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          nodesAndSeparators.Add(comma)
        Else
          parseNextParameter = False
        End If

      End While

      Return New SeparatedSyntaxList(Of ParameterSyntax)(nodesAndSeparators.ToImmutable)

    End Function

    Private Function ParseParameter() As ParameterSyntax
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim type = ParseTypeClause()
      Return New ParameterSyntax(m_syntaxTree, identifier, type)
    End Function

    Private Function ParseGlobalStatement() As MemberSyntax
      Dim statement = ParseStatement()
      Return New GlobalStatementSyntax(m_syntaxTree, statement)
    End Function

    Private Function ParseStatement() As StatementSyntax
      Select Case Current.Kind
        Case SyntaxKind.OpenBraceToken
          Return ParseBlockStatement
        Case SyntaxKind.LetKeyword,
             SyntaxKind.VarKeyword,
             SyntaxKind.LetKeyword
          Return ParseVariableDeclaration
        Case SyntaxKind.IfKeyword
          Return ParseIfStatement
        Case SyntaxKind.WhileKeyword
          Return ParseWhileStatement
        Case SyntaxKind.DoKeyword
          Return ParseDoWhileStatement
        Case SyntaxKind.ForKeyword
          Return ParseForStatement
        Case SyntaxKind.BreakKeyword
          Return ParseBreakStatement
        Case SyntaxKind.ContinueKeyword
          Return ParseContinueStatement
        Case SyntaxKind.ReturnKeyword
          Return ParseReturnStatement
        Case Else
          Return ParseExpressionStatement
      End Select
    End Function

    Private Function ParseBlockStatement() As StatementSyntax

      Dim statements = ImmutableArray.CreateBuilder(Of StatementSyntax)

      Dim openBraceToken = MatchToken(SyntaxKind.OpenBraceToken)

      While Current.Kind <> SyntaxKind.EndOfFileToken AndAlso
            Current.Kind <> SyntaxKind.CloseBraceToken

        Dim startToken = Current

        Dim statement = ParseStatement()
        statements.Add(statement)

        ' If ParseStatement() did not consume any tokens,
        ' we need to skip the current token and continue
        ' in order to avoid an infinite loop.
        ' We don't need to report an error because we'll
        ' already tried to parse an expression statement
        ' and reported one.
        If (Current Is startToken) Then
          NextToken()
        End If

      End While
      Dim closeBraceToken = MatchToken(SyntaxKind.CloseBraceToken)

      Return New BlockStatementSyntax(m_syntaxTree, openBraceToken, statements.ToImmutable, closeBraceToken)

    End Function

    Private Function ParseVariableDeclaration() As StatementSyntax

      ' The following line is modified from the original in order to
      ' allow the addition of the DIM keyword (in addition to LET and VAR).
      'Dim expected = If(Me.Current.Kind = SyntaxKind.LetKeyword, SyntaxKind.LetKeyword, SyntaxKind.VarKeyword)
      Dim expected = SyntaxKind.VarKeyword
      ' If LET or DIM, set... otherwise, default to VAR (whether it's VAR or not).
      Select Case Current.Kind
        Case SyntaxKind.LetKeyword : expected = SyntaxKind.LetKeyword
        Case Else
      End Select

      Dim keyword = MatchToken(expected)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim typeClause = ParseOptionalTypeClause()
      Dim equals = MatchToken(SyntaxKind.EqualsToken)
      Dim initializer = ParseExpression()

      Return New VariableDeclarationSyntax(m_syntaxTree, keyword, identifier, typeClause, equals, initializer)

    End Function

    Private Function ParseOptionalTypeClause() As TypeClauseSyntax
      If Current.Kind <> SyntaxKind.ColonToken Then
        Return Nothing
      End If
      Return ParseTypeClause()
    End Function

    Private Function ParseTypeClause() As TypeClauseSyntax
      Dim colonToken = MatchToken(SyntaxKind.ColonToken)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Return New TypeClauseSyntax(m_syntaxTree, colonToken, identifier)
    End Function

    Private Function ParseIfStatement() As StatementSyntax
      Dim keyword = MatchToken(SyntaxKind.IfKeyword)
      Dim condition = ParseExpression
      Dim statement = ParseStatement()
      Dim elseClause = ParseElseClause()
      Return New IfStatementSyntax(m_syntaxTree, keyword, condition, statement, elseClause)
    End Function

    Private Function ParseElseClause() As ElseClauseSyntax
      If Current.Kind <> SyntaxKind.ElseKeyword Then
        Return Nothing
      End If
      Dim keyword = NextToken
      Dim statement = ParseStatement()
      Return New ElseClauseSyntax(m_syntaxTree, keyword, statement)
    End Function

    Private Function ParseWhileStatement() As StatementSyntax
      Dim keyword = MatchToken(SyntaxKind.WhileKeyword)
      Dim condition = ParseExpression
      Dim body = ParseStatement()
      Return New WhileStatementSyntax(m_syntaxTree, keyword, condition, body)
    End Function

    Private Function ParseDoWhileStatement() As StatementSyntax
      Dim doKeyword = MatchToken(SyntaxKind.DoKeyword)
      Dim body = ParseStatement
      Dim whileKeyword = MatchToken(SyntaxKind.WhileKeyword)
      Dim condition = ParseExpression()
      Return New DoWhileStatementSyntax(m_syntaxTree, doKeyword, body, whileKeyword, condition)
    End Function

    Private Function ParseForStatement() As StatementSyntax
      Dim keyword = MatchToken(SyntaxKind.ForKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim equalsToken = MatchToken(SyntaxKind.EqualsToken)
      Dim lowerBound = ParseExpression
      Dim toKeyword = MatchToken(SyntaxKind.ToKeyword)
      Dim upperBound = ParseExpression
      Dim body = ParseStatement()
      Return New ForStatementSyntax(m_syntaxTree, keyword, identifier, equalsToken, lowerBound, toKeyword, upperBound, body)
    End Function

    Private Function ParseBreakStatement() As StatementSyntax
      Dim keyword = MatchToken(SyntaxKind.BreakKeyword)
      Return New BreakStatementSyntax(m_syntaxTree, keyword)
    End Function

    Private Function ParseContinueStatement() As StatementSyntax
      Dim keyword = MatchToken(SyntaxKind.ContinueKeyword)
      Return New ContinueStatementSyntax(m_syntaxTree, keyword)
    End Function

    Private Function ParseReturnStatement() As StatementSyntax
      Dim keyword = MatchToken(SyntaxKind.ReturnKeyword)
      Dim keywordLine = Text.GetLineIndex(keyword.Span.Start)
      Dim currentLine = Text.GetLineIndex(Current.Span.Start)
      Dim isEof = (Me.Current.Kind = SyntaxKind.EndOfFileToken)
      Dim sameLine = Not isEof AndAlso keywordLine = currentLine
      Dim expression = If(sameLine, ParseExpression, Nothing)
      Return New ReturnStatementSyntax(m_syntaxTree, keyword, expression)
    End Function

    Private Function ParseExpressionStatement() As ExpressionStatementSyntax
      Dim expression = ParseExpression()
      Return New ExpressionStatementSyntax(m_syntaxTree, expression)
    End Function

    Private Function ParseExpression() As ExpressionSyntax
      Return ParseAssignmentExpression
    End Function

    Private Function ParseAssignmentExpression() As ExpressionSyntax

      If (Me.Peek(0).Kind = SyntaxKind.IdentifierToken AndAlso
          Me.Peek(1).Kind = SyntaxKind.EqualsToken) Then

        Dim identifierToken = NextToken
        Dim operatorToken = NextToken
        Dim right = Me.ParseAssignmentExpression
        Return New AssignmentExpressionSyntax(m_syntaxTree, identifierToken, operatorToken, right)

      End If

      Return ParseBinaryExpression

    End Function

    Private Function ParseBinaryExpression(Optional parentPrecedence As Integer = 0) As ExpressionSyntax

      Dim left As ExpressionSyntax
      Dim unaryOperatorPrecedence = Current.Kind.GetUnaryOperatorPrecedence
      If unaryOperatorPrecedence <> 0 AndAlso unaryOperatorPrecedence >= parentPrecedence Then
        Dim operatorToken = NextToken()
        Dim operand = ParseBinaryExpression(unaryOperatorPrecedence)
        left = New UnaryExpressionSyntax(m_syntaxTree, operatorToken, operand)
      Else
        left = ParsePrimaryExpression
      End If

      While True

        Dim precedence = Current.Kind.GetBinaryOperatorPrecedence
        If precedence = 0 OrElse precedence <= parentPrecedence Then
          Exit While
        End If
        Dim operatorToken = NextToken()
        Dim right = ParseBinaryExpression(precedence)
        left = New BinaryExpressionSyntax(m_syntaxTree, left, operatorToken, right)

      End While

      Return left

    End Function

    Private Function ParsePrimaryExpression() As ExpressionSyntax

      Select Case Current.Kind
        Case SyntaxKind.OpenParenToken : Return ParseParenExpression
        Case SyntaxKind.FalseKeyword : Return ParseBooleanLiteral
        Case SyntaxKind.TrueKeyword : Return ParseBooleanLiteral
        Case SyntaxKind.NumberToken : Return ParseNumberLiteral
        Case SyntaxKind.StringToken : Return ParseStringLiteral
        Case SyntaxKind.IdentifierToken : Return ParseNameorCallExpression
        Case Else
          ' Default to parsing a name expression if we reach this far.
          Return ParseNameOrCallExpression
      End Select

    End Function

    Private Function ParseParenExpression() As ExpressionSyntax
      Dim left = MatchToken(SyntaxKind.OpenParenToken)
      Dim expression = ParseExpression
      Dim right = MatchToken(SyntaxKind.CloseParenToken)
      Return New ParenExpressionSyntax(m_syntaxTree, left, expression, right)
    End Function

    Private Function ParseBooleanLiteral() As ExpressionSyntax
      Dim isTrue = (Me.Current.Kind = SyntaxKind.TrueKeyword)
      Dim keywordToken = MatchToken(If(isTrue, SyntaxKind.TrueKeyword, SyntaxKind.FalseKeyword))
      Return New LiteralExpressionSyntax(m_syntaxTree, keywordToken, isTrue)
    End Function

    Private Function ParseNumberLiteral() As ExpressionSyntax
      Dim numberToken = MatchToken(SyntaxKind.NumberToken)
      Return New LiteralExpressionSyntax(m_syntaxTree, numberToken)
    End Function

    Private Function ParseStringLiteral() As ExpressionSyntax
      Dim stringToken = MatchToken(SyntaxKind.StringToken)
      Return New LiteralExpressionSyntax(m_syntaxTree, stringToken)
    End Function

    Private Function ParseNameOrCallExpression() As ExpressionSyntax
      If Me.Peek(0).Kind = SyntaxKind.IdentifierToken AndAlso
         Me.Peek(1).Kind = SyntaxKind.OpenParenToken Then
        Return ParseCallExpression
      Else
        Return ParseNameExpression
      End If
    End Function

    Private Function ParseCallExpression() As ExpressionSyntax
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
      Dim arguments = ParseArguments
      Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
      Return New CallExpressionSyntax(m_syntaxTree, identifier, openParen, arguments, closeParen)
    End Function

    Private Function ParseArguments() As SeparatedSyntaxList(Of ExpressionSyntax)

      Dim nodesAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxNode)

      Dim parseNextArgument = True
      While parseNextArgument AndAlso
            Current.Kind <> SyntaxKind.CloseParenToken AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken

        Dim expression = ParseExpression
        nodesAndSeparators.Add(expression)

        If Me.Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          nodesAndSeparators.Add(comma)
        Else
          parseNextArgument = False
        End If

      End While

      Return New SeparatedSyntaxList(Of ExpressionSyntax)(nodesAndSeparators.ToImmutable)

    End Function

    Private Function ParseNameExpression() As ExpressionSyntax
      Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
      Return New NameExpressionSyntax(m_syntaxTree, identifierToken)
    End Function

  End Class

End Namespace