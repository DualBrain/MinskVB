﻿Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class Binder

    Private m_scope As BoundScope

    Public Sub New(parent As BoundScope)
      Me.m_scope = New BoundScope(parent)
    End Sub

    Public Shared Function BindGlobalScope(previous As BoundGlobalScope, syntax As CompilationUnitSyntax) As BoundGlobalScope

      Dim parentScope = CreateParentScopes(previous)
      Dim binder = New Binder(parentScope)
      Dim expression = binder.BindStatement(syntax.Statement)
      Dim variables = binder.m_scope.GetDeclaredVariables
      Dim diagnostics = binder.Diagnostics.ToImmutableArray

      If previous IsNot Nothing Then
        diagnostics = diagnostics.InsertRange(0, previous.Diagnostics)
      End If

      Return New BoundGlobalScope(previous, diagnostics, variables, expression)

    End Function

    Private Shared Function CreateParentScopes(previous As BoundGlobalScope) As BoundScope

      Dim stack = New Stack(Of BoundGlobalScope)

      While previous IsNot Nothing
        stack.Push(previous)
        previous = previous.Previous
      End While

      Dim parent = CreateRootScope()

      While stack.Count > 0
        previous = stack.Pop
        Dim scope = New BoundScope(parent)
        For Each v In previous.Variables
          scope.TryDeclareVariable(v)
        Next
        parent = scope
      End While

      Return parent

    End Function

    Private Shared Function CreateRootScope() As BoundScope
      Dim result = New BoundScope(Nothing)
      For Each f In BuiltinFunctions.GetAll
        result.TryDeclareFunction(f)
      Next
      Return result
    End Function

    Public ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag

    Private Function BindExpression(syntax As ExpressionSyntax, targetType As TypeSymbol) As BoundExpression
      Return Me.BindConversion(syntax, targetType)
    End Function

    Public Function BindExpression(syntax As ExpressionSyntax, Optional canBeVoid As Boolean = False) As BoundExpression
      Dim result = Me.BindExpressionInternal(syntax)
      If Not canBeVoid AndAlso result.Type Is TypeSymbol.Void Then
        Me.Diagnostics.ReportExpressionMustHaveValue(syntax.Span)
        Return New BoundErrorExpression
      End If
      Return result
    End Function

    Public Function BindExpressionInternal(syntax As ExpressionSyntax) As BoundExpression

      Select Case syntax.Kind
        Case SyntaxKind.ParenExpression
          Return Me.BindParenExpression(DirectCast(syntax, ParenExpressionSyntax))
        Case SyntaxKind.LiteralExpression
          Return Me.BindLiteralExpression(DirectCast(syntax, LiteralExpressionSyntax))
        Case SyntaxKind.NameExpression
          Return Me.BindNameExpression(DirectCast(syntax, NameExpressionSyntax))
        Case SyntaxKind.AssignmentExpression
          Return Me.BindAssignmentExpression(DirectCast(syntax, AssignmentExpressionSyntax))
        Case SyntaxKind.UnaryExpression
          Return Me.BindUnaryEpression(DirectCast(syntax, UnaryExpressionSyntax))
        Case SyntaxKind.BinaryExpression
          Return Me.BindBinaryEpression(DirectCast(syntax, BinaryExpressionSyntax))
        Case SyntaxKind.CallExpression
          Return Me.BindCallEpression(DirectCast(syntax, CallExpressionSyntax))
        Case Else
          Throw New Exception($"Unexpected syntax {syntax.Kind}")
      End Select

    End Function

    Private Function BindStatement(syntax As StatementSyntax) As BoundStatement

      Select Case syntax.Kind
        Case SyntaxKind.BlockStatement
          Return Me.BindBlockStatement(DirectCast(syntax, BlockStatementSyntax))
        Case SyntaxKind.VariableDeclaration
          Return Me.BindVariableDeclaration(DirectCast(syntax, VariableDeclarationSyntax))
        Case SyntaxKind.IfStatement
          Return Me.BindIfStatement(DirectCast(syntax, IfStatementSyntax))
        Case SyntaxKind.WhileStatement
          Return Me.BindWhileStatement(DirectCast(syntax, WhileStatementSyntax))
        Case SyntaxKind.DoWhileStatement
          Return Me.BindDoWhileStatement(DirectCast(syntax, DoWhileStatementSyntax))
        Case SyntaxKind.ForStatement
          Return Me.BindForStatement(DirectCast(syntax, ForStatementSyntax))
        Case SyntaxKind.ExpressionStatement
          Return Me.BindExpressionStatement(DirectCast(syntax, ExpressionStatementSyntax))
        Case Else
          Throw New Exception($"Unexpected syntax {syntax.Kind}")
      End Select

    End Function

    Private Function BindBlockStatement(syntax As BlockStatementSyntax) As BoundStatement
      Dim statements = ImmutableArray.CreateBuilder(Of BoundStatement)
      Me.m_scope = New BoundScope(Me.m_scope)
      For Each statementSyntax In syntax.Statements
        Dim statement = Me.BindStatement(statementSyntax)
        statements.Add(statement)
      Next
      Me.m_scope = Me.m_scope.Parent
      Return New BoundBlockStatement(statements.ToImmutable)
    End Function

    Private Function BindVariableDeclaration(syntax As VariableDeclarationSyntax) As BoundStatement
      Dim isReadOnly = syntax.Keyword.Kind = SyntaxKind.LetKeyword
      Dim initializer = Me.BindExpression(syntax.Initializer)
      Dim variable = Me.BindVariable(syntax.Identifier, isReadOnly, initializer.Type)
      Return New BoundVariableDeclaration(variable, initializer)
    End Function

    Private Function BindIfStatement(syntax As IfStatementSyntax) As BoundStatement
      Dim condition = Me.BindExpression(syntax.Condition, TypeSymbol.Bool)
      Dim thenStatement = Me.BindStatement(syntax.ThenStatement)
      Dim elseStatement = If(syntax.ElseClause IsNot Nothing, Me.BindStatement(syntax.ElseClause.ElseStatement), Nothing)
      Return New BoundIfStatement(condition, thenStatement, elseStatement)
    End Function

    Private Function BindWhileStatement(syntax As WhileStatementSyntax) As BoundStatement
      Dim condition = Me.BindExpression(syntax.Condition, TypeSymbol.Bool)
      Dim body = Me.BindStatement(syntax.Body)
      Return New BoundWhileStatement(condition, body)
    End Function

    Private Function BindDoWhileStatement(syntax As DoWhileStatementSyntax) As BoundStatement
      Dim body = Me.BindStatement(syntax.Body)
      Dim condition = Me.BindExpression(syntax.Condition, TypeSymbol.Bool)
      Return New BoundDoWhileStatement(body, condition)
    End Function

    Private Function BindForStatement(syntax As ForStatementSyntax) As BoundStatement

      Dim lowerBound = Me.BindExpression(syntax.LowerBound, TypeSymbol.Int)
      Dim upperBound = Me.BindExpression(syntax.UpperBound, TypeSymbol.Int)

      Me.m_scope = New BoundScope(Me.m_scope)

      Dim variable = Me.BindVariable(syntax.Identifier, True, TypeSymbol.Int)
      Dim body = Me.BindStatement(syntax.Body)

      Me.m_scope = Me.m_scope.Parent

      Return New BoundForStatement(variable, lowerBound, upperBound, body)

    End Function

    Private Function BindExpressionStatement(syntax As ExpressionStatementSyntax) As BoundStatement
      Dim expression = Me.BindExpression(syntax.Expression, True)
      Return New BoundExpressionStatement(expression)
    End Function

    Private Function BindParenExpression(syntax As ParenExpressionSyntax) As BoundExpression
      Return Me.BindExpression(syntax.Expression)
    End Function

    Private Function BindLiteralExpression(syntax As LiteralExpressionSyntax) As BoundExpression
      Dim value = If(syntax.Value, 0)
      Return New BoundLiteralExpression(value)
    End Function

    Private Function BindNameExpression(syntax As NameExpressionSyntax) As BoundExpression
      Dim name = syntax.IdentifierToken.Text
      If syntax.IdentifierToken.IsMissing Then
        ' This means the token was inserted by the parser.  We already
        ' reported error so we can just return an error expression.
        Return New BoundErrorExpression
      End If
      Dim variable As VariableSymbol = Nothing
      If Not Me.m_scope.TryLookupVariable(name, variable) Then
        Me.Diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name)
        Return New BoundErrorExpression
      End If
      Return New BoundVariableExpression(variable)
    End Function

    Private Function BindAssignmentExpression(syntax As AssignmentExpressionSyntax) As BoundExpression

      Dim name = syntax.IdentifierToken.Text
      Dim boundExpression = Me.BindExpression(syntax.Expression)

      Dim variable As VariableSymbol = Nothing
      If Not Me.m_scope.TryLookupVariable(name.ToLower, variable) Then
        Me.Diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name)
        Return boundExpression
      End If

      If variable.IsReadOnly Then
        Me.Diagnostics.ReportCannotAssign(syntax.EqualsToken.Span, name)
      End If

      Dim convertedExpression = Me.BindConversion(syntax.Expression.Span, boundExpression, variable.Type)

      Return New BoundAssignmentExpression(variable, convertedExpression)

    End Function

    Private Function BindUnaryEpression(syntax As UnaryExpressionSyntax) As BoundExpression
      Dim boundOperand = Me.BindExpression(syntax.Operand)
      If boundOperand.Type Is TypeSymbol.Error Then
        Return New BoundErrorExpression
      End If
      Dim boundOperator = BoundUnaryOperator.Bind(syntax.OperatorToken.Kind, boundOperand.Type)
      If boundOperator Is Nothing Then
        Me.Diagnostics.ReportUndefinedUnaryOperator(syntax.OperatorToken.Span, syntax.OperatorToken.Text, boundOperand.Type)
        Return New BoundErrorExpression
      End If
      Return New BoundUnaryExpression(boundOperator, boundOperand)
    End Function

    Private Function BindBinaryEpression(syntax As BinaryExpressionSyntax) As BoundExpression
      Dim boundLeft = Me.BindExpression(syntax.Left)
      Dim boundRight = Me.BindExpression(syntax.Right)
      If boundLeft.Type Is TypeSymbol.Error OrElse boundRight.Type Is TypeSymbol.Error Then
        Return New BoundErrorExpression
      End If
      Dim boundOperator = BoundBinaryOperator.Bind(syntax.OperatorToken.Kind, boundLeft.Type, boundRight.Type)
      If boundOperator Is Nothing Then
        Me.Diagnostics.ReportUndefinedBinaryOperator(syntax.OperatorToken.Span, syntax.OperatorToken.Text, boundLeft.Type, boundRight.Type)
        Return New BoundErrorExpression
      End If
      Return New BoundBinaryExpression(boundLeft, boundOperator, boundRight)
    End Function

    Private Function BindCallEpression(syntax As CallExpressionSyntax) As BoundExpression

      Dim t = Me.LookupType(syntax.Identifier.Text)
      If syntax.Arguments.Count = 1 AndAlso TypeOf t Is TypeSymbol Then
        Return Me.BindConversion(syntax.Arguments(0), t)
      End If

      Dim boundArguments = ImmutableArray.CreateBuilder(Of BoundExpression)

      For Each argument In syntax.Arguments
        Dim boundArgument = Me.BindExpression(argument)
        boundArguments.Add(boundArgument)
      Next

      Dim func As FunctionSymbol = Nothing

      If Not Me.m_scope.TryLookupFunction(syntax.Identifier.Text, func) Then
        Me.Diagnostics.ReportUndefinedFunction(syntax.Identifier.Span, syntax.Identifier.Text)
        Return New BoundErrorExpression
      End If
      If syntax.Arguments.Count <> func.Parameters.Length Then
        Me.Diagnostics.ReportWrongArgumentCount(syntax.Span, func.Name, func.Parameters.Length, syntax.Arguments.Count)
        Return New BoundErrorExpression
      End If

      For i = 0 To syntax.Arguments.Count - 1
        Dim argument = boundArguments(i)
        Dim parameter = func.Parameters(i)
        If argument.Type IsNot parameter.Type Then
          Me.Diagnostics.ReportWrongArgumentType(syntax.Span, parameter.Name, parameter.Type, argument.Type)
          Return New BoundErrorExpression
        End If
      Next

      Return New BoundCallExpression(func, boundArguments.ToImmutableArray)

    End Function

    Private Function BindConversion(syntax As ExpressionSyntax, [type] As TypeSymbol) As BoundExpression
      Dim expression = Me.BindExpression(syntax)
      Return Me.BindConversion(syntax.Span, expression, type)
    End Function

    Private Function BindConversion(diagnosticSpan As Text.TextSpan, expression As BoundExpression, type As TypeSymbol) As BoundExpression
      Dim c = Conversion.Classify(expression.Type, [type])
      If Not c.Exists Then
        If expression.Type IsNot TypeSymbol.Error AndAlso [type] IsNot TypeSymbol.Error Then
          Me.Diagnostics.ReportCannotConvert(diagnosticSpan, expression.Type, [type])
        End If
        Return New BoundErrorExpression
      End If
      If c.IsIdentity Then Return expression
      Return New BoundConversionExpression([type], expression)
    End Function

    Private Function BindVariable(identifier As SyntaxToken, isReadOnly As Boolean, type As TypeSymbol) As VariableSymbol
      Dim name = If(identifier.Text, "?")
      Dim [declare] = Not identifier.IsMissing
      Dim variable = New VariableSymbol(name, isReadOnly, type)
      If [declare] AndAlso Not Me.m_scope.TryDeclareVariable(variable) Then
        Me.Diagnostics.ReportSymbolAlreadyDeclared(identifier.Span, name)
      End If
      Return variable
    End Function

    Private Function LookupType(name As String) As TypeSymbol
      Select Case name
        Case "bool" : Return TypeSymbol.Bool
        Case "int" : Return TypeSymbol.Int
        Case "string" : Return TypeSymbol.String
        Case Else
          Return Nothing
      End Select
    End Function

  End Class

End Namespace