Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Lowering
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class Binder

    Private m_scope As BoundScope
    Private ReadOnly m_function As FunctionSymbol

    Public Sub New(parent As BoundScope, [function] As FunctionSymbol)

      Me.m_scope = New BoundScope(parent)
      Me.m_function = [function]

      If [function] IsNot Nothing Then
        For Each p In [function].Parameters
          Me.m_scope.TryDeclareVariable(p)
        Next
      End If

    End Sub

    Public Shared Function BindGlobalScope(previous As BoundGlobalScope, syntax As CompilationUnitSyntax) As BoundGlobalScope

      Dim parentScope = CreateParentScopes(previous)
      Dim binder = New Binder(parentScope, Nothing)

      'Dim statement = syntax.Members.OfType(Of GlobalStatementSyntax).FirstOrDefault
      For Each func In syntax.Members.OfType(Of FunctionDeclarationSyntax)
        binder.BindFunctionDeclaration(func)
      Next

      Dim statementBuilder = ImmutableArray.CreateBuilder(Of BoundStatement)

      For Each globalStatement In syntax.Members.OfType(Of GlobalStatementSyntax)
        Dim s = binder.BindStatement(globalStatement.Statement)
        statementBuilder.Add(s)
      Next

      Dim statement = New BoundBlockStatement(statementBuilder.ToImmutable)

      Dim functions = binder.m_scope.GetDeclaredFunctions
      Dim variables = binder.m_scope.GetDeclaredVariables
      Dim diagnostics = binder.Diagnostics.ToImmutableArray

      If previous IsNot Nothing Then
        diagnostics = diagnostics.InsertRange(0, previous.Diagnostics)
      End If

      Return New BoundGlobalScope(previous, diagnostics, functions, variables, statement)

    End Function

    Public Shared Function BindProgram(globalScope As BoundGlobalScope) As BoundProgram

      Dim parentScope = CreateParentScopes(globalScope)

      Dim functionBodies = ImmutableDictionary.CreateBuilder(Of FunctionSymbol, BoundBlockStatement)
      Dim diagnostics = New DiagnosticBag

      Dim scope = globalScope
      While scope IsNot Nothing

        For Each func In scope.Functions
          Dim binder = New Binder(parentScope, func)
          Dim body = binder.BindStatement(func.Declaration.Body)
          Dim loweredBody = Lowerer.Lower(body)
          functionBodies.Add(func, loweredBody)
          diagnostics.AddRange(binder.Diagnostics)
        Next

        scope = scope.Previous

      End While

      Return New BoundProgram(globalScope, diagnostics, functionBodies.ToImmutable)

    End Function


    Private Sub BindFunctionDeclaration(syntax As FunctionDeclarationSyntax)
      Dim parameters = ImmutableArray.CreateBuilder(Of ParameterSymbol)
      Dim seenParameterNames = New HashSet(Of String)
      For Each parameterSyntax In syntax.Parameters
        Dim parameterName = parameterSyntax.Identifier.Text
        Dim parameterType = Me.BindTypeClause(parameterSyntax.Type)
        If Not seenParameterNames.Add(parameterName) Then
          Me.Diagnostics.ReportParameterAlreadyDeclared(parameterSyntax.Span, parameterName)
        Else
          Dim parameter = New ParameterSymbol(parameterName, parameterType)
          parameters.Add(parameter)
        End If
      Next
      Dim type = If(Me.BindTypeClause(syntax.Type), TypeSymbol.Void)
      If type IsNot TypeSymbol.Void Then
        Me.Diagnostics.XXX_ReportFunctionsAreUnsupported(syntax.Type.Span)
      End If
      Dim func = New FunctionSymbol(syntax.Identifier.Text, parameters.ToImmutable, type, syntax)
      If Not Me.m_scope.TryDeclareFunction(func) Then
        Me.Diagnostics.ReportSymbolAlreadyDeclared(syntax.Identifier.Span, func.Name)
      End If
    End Sub

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
        For Each f In previous.Functions
          scope.TryDeclareFunction(f)
        Next
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
      Dim type = Me.BindTypeClause(syntax.TypeClause)
      Dim initializer = Me.BindExpression(syntax.Initializer)
      Dim variableType = If(type, initializer.Type)
      Dim variable = Me.BindVariable(syntax.Identifier, isReadOnly, variableType)
      Dim convertedInitializer = Me.BindConversion(syntax.Initializer.Span, initializer, variableType)
      Return New BoundVariableDeclaration(variable, convertedInitializer)
    End Function

    Private Function BindTypeClause(syntax As TypeClauseSyntax) As TypeSymbol
      If syntax Is Nothing Then
        Return Nothing
      End If
      Dim type = Me.LookupType(syntax.Identifier.Text)
      If type Is Nothing Then
        Me.Diagnostics.ReportUndefinedType(syntax.Identifier.Span, syntax.Identifier.Text)
        Return Nothing
      End If
      Return type
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
        Return Me.BindConversion(syntax.Arguments(0), t, True)
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
          'Me.Diagnostics.ReportWrongArgumentType(syntax.Span, parameter.Name, parameter.Type, argument.Type)
          Me.Diagnostics.ReportWrongArgumentType(syntax.Arguments(i).Span, parameter.Name, parameter.Type, argument.Type)
          Return New BoundErrorExpression
        End If
      Next

      Return New BoundCallExpression(func, boundArguments.ToImmutableArray)

    End Function

    Private Function BindConversion(syntax As ExpressionSyntax, [type] As TypeSymbol, Optional allowExplicit As Boolean = False) As BoundExpression
      Dim expression = Me.BindExpression(syntax)
      Return Me.BindConversion(syntax.Span, expression, type, allowExplicit)
    End Function

    Private Function BindConversion(diagnosticSpan As Text.TextSpan,
                                        expression As BoundExpression,
                                        type As TypeSymbol,
                                        Optional allowExplicit As Boolean = False) As BoundExpression
      Dim c = Conversion.Classify(expression.Type, [type])
      If Not c.Exists Then
        If expression.Type IsNot TypeSymbol.Error AndAlso [type] IsNot TypeSymbol.Error Then
          Me.Diagnostics.ReportCannotConvert(diagnosticSpan, expression.Type, [type])
        End If
        Return New BoundErrorExpression
      End If
      If Not allowExplicit AndAlso c.IsExplicit Then
        Me.Diagnostics.ReportCannotConvertImplicitly(diagnosticSpan, expression.Type, [type])
      End If

      If c.IsIdentity Then Return expression
      Return New BoundConversionExpression([type], expression)
    End Function

    Private Function BindVariable(identifier As SyntaxToken, isReadOnly As Boolean, type As TypeSymbol) As VariableSymbol
      Dim name = If(identifier.Text, "?")
      Dim [declare] = Not identifier.IsMissing
      Dim variable = If(Me.m_function Is Nothing,
                              DirectCast(New GlobalVariableSymbol(name, isReadOnly, type), VariableSymbol),
                              DirectCast(New LocalVariableSymbol(name, isReadOnly, type), VariableSymbol))
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