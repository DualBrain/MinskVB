Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Lowering
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class Binder

    Private m_scope As BoundScope
    Private ReadOnly m_isScript As Boolean
    Private ReadOnly m_function As FunctionSymbol
    Private ReadOnly Property m_diagnostics As DiagnosticBag = New DiagnosticBag
    Private ReadOnly m_loopStack As New Stack(Of (BreakLabel As BoundLabel, ContinueLabel As BoundLabel))
    Private m_labelCounter As Integer

    Private Sub New(isScript As Boolean, parent As BoundScope, [function] As FunctionSymbol)

      m_scope = New BoundScope(parent)
      m_isScript = isScript
      m_function = [function]

      If [function] IsNot Nothing Then
        For Each p In [function].Parameters
          m_scope.TryDeclareVariable(p)
        Next
      End If

    End Sub

    Public Shared Function BindGlobalScope(isScript As Boolean, previous As BoundGlobalScope, syntaxTrees As ImmutableArray(Of SyntaxTree)) As BoundGlobalScope

      Dim parentScope = CreateParentScopes(previous)
      Dim binder = New Binder(isScript, parentScope, Nothing)

      binder.m_diagnostics.AddRange(syntaxTrees.SelectMany(Function(st) st.Diagnostics))
      If binder.m_diagnostics.Any Then
        Return New BoundGlobalScope(previous, binder.m_diagnostics.ToImmutableArray, Nothing, Nothing, ImmutableArray(Of FunctionSymbol).Empty, ImmutableArray(Of VariableSymbol).Empty, ImmutableArray(Of BoundStatement).Empty)
      End If

      Dim functionDeclarations = syntaxTrees.SelectMany(Function(st) st.Root.Members).OfType(Of FunctionDeclarationSyntax)

      For Each func In functionDeclarations
        binder.BindFunctionDeclaration(func)
      Next

      Dim globalStatements = syntaxTrees.SelectMany(Function(st) st.Root.Members).OfType(Of GlobalStatementSyntax)

      Dim statements = ImmutableArray.CreateBuilder(Of BoundStatement)
      For Each globalStatement In globalStatements
        Dim statement = binder.BindGlobalStatement(globalStatement.Statement)
        statements.Add(statement)
      Next

      ' Check global statements.

      Dim firstGlobalStatementPerSyntaxTree = syntaxTrees.Select(Function(st) st.Root.Members.OfType(Of GlobalStatementSyntax).FirstOrDefault).
                                                          Where(Function(g) g IsNot Nothing).
                                                          ToArray

      If firstGlobalStatementPerSyntaxTree.Length > 1 Then
        For Each globalStatement In firstGlobalStatementPerSyntaxTree
          binder.m_diagnostics.ReportOnlyOneFileCanHaveGlobalStatements(globalStatement.Location)
        Next
      End If

      ' Check for main/script with global statements.

      Dim functions = binder.m_scope.GetDeclaredFunctions

      Dim mainFunction As FunctionSymbol
      Dim scriptFunction As FunctionSymbol

      If isScript Then

        mainFunction = Nothing

        If globalStatements.Any Then
          scriptFunction = New FunctionSymbol("$eval", ImmutableArray(Of ParameterSymbol).Empty, TypeSymbol.Any, Nothing)
        Else
          scriptFunction = Nothing
        End If

      Else

        mainFunction = functions.FirstOrDefault(Function(f) f.Name = "main")
        scriptFunction = Nothing

        If mainFunction IsNot Nothing Then
          If mainFunction.Type IsNot TypeSymbol.Void OrElse
             mainFunction.Parameters.Any Then
            binder.m_diagnostics.ReportMainMustHaveCorrectSignature(mainFunction.Declaration.Identifier.Location)
          End If
        End If

        If globalStatements.Any Then
          If mainFunction IsNot Nothing Then
            binder.m_diagnostics.ReportCannotMixMainAndGlobalStatements(mainFunction.Declaration.Identifier.Location)
            For Each globalStatement In firstGlobalStatementPerSyntaxTree
              binder.m_diagnostics.ReportCannotMixMainAndGlobalStatements(globalStatement.Location)
            Next
          Else
            mainFunction = New FunctionSymbol("main", ImmutableArray(Of ParameterSymbol).Empty, TypeSymbol.Void)
          End If
        End If

      End If

      Dim diagnostics = binder.m_diagnostics.ToImmutableArray

      Dim variables = binder.m_scope.GetDeclaredVariables

      If previous IsNot Nothing Then
        diagnostics = diagnostics.InsertRange(0, previous.Diagnostics)
      End If

      Return New BoundGlobalScope(previous, diagnostics, mainFunction, scriptFunction, functions, variables, statements.ToImmutable)

    End Function

    Public Shared Function BindProgram(isScript As Boolean, previous As BoundProgram, globalScope As BoundGlobalScope) As BoundProgram

      Dim parentScope = CreateParentScopes(globalScope)

      If globalScope.Diagnostics.Any Then
        Return New BoundProgram(previous, globalScope.Diagnostics, Nothing, Nothing, ImmutableDictionary(Of FunctionSymbol, BoundBlockStatement).Empty)
      End If

      Dim functionBodies = ImmutableDictionary.CreateBuilder(Of FunctionSymbol, BoundBlockStatement)
      Dim diagnostics = ImmutableArray.CreateBuilder(Of Diagnostic)

      For Each func In globalScope.Functions
        Dim binder = New Binder(isScript, parentScope, func)
        Dim body = binder.BindStatement(func.Declaration.Body)
        Dim loweredBody = Lowerer.Lower(func, body)
        If func.Type IsNot TypeSymbol.Void AndAlso Not ControlFlowGraph.AllPathsReturn(loweredBody) Then
          binder.m_diagnostics.ReportAllPathsMustReturn(func.Declaration.Identifier.Location)
        End If
        functionBodies.Add(func, loweredBody)
        diagnostics.AddRange(binder.m_diagnostics)
      Next

      If globalScope.MainFunction IsNot Nothing AndAlso globalScope.Statements.Any Then
        Dim body = Lowerer.Lower(globalScope.MainFunction, New BoundBlockStatement(globalScope.Statements))
        functionBodies.Add(globalScope.MainFunction, body)
      ElseIf globalScope.ScriptFunction IsNot Nothing Then
        Dim statements = globalScope.Statements
        Dim es = TryCast(statements(0), BoundExpressionStatement)
        Dim needsReturn = statements.Length = 1 AndAlso
                          TypeOf es Is BoundExpressionStatement AndAlso
                          es.Expression.Type IsNot TypeSymbol.Void
        If needsReturn Then
          statements = statements.SetItem(0, New BoundReturnStatement(es.Expression))
        ElseIf statements.Any AndAlso
               statements.Last.Kind <> BoundNodeKind.ReturnStatement Then
          Dim nullValue = New BoundLiteralExpression("")
          statements = statements.Add(New BoundReturnStatement(nullValue))
        End If
        Dim body = Lowerer.Lower(globalScope.ScriptFunction, New BoundBlockStatement(statements))
        functionBodies.Add(globalScope.ScriptFunction, body)
      End If

      Return New BoundProgram(previous, diagnostics.ToImmutable, globalScope.MainFunction, globalScope.ScriptFunction, functionBodies.ToImmutable)

    End Function


    Private Sub BindFunctionDeclaration(syntax As FunctionDeclarationSyntax)
      Dim parameters = ImmutableArray.CreateBuilder(Of ParameterSymbol)
      Dim seenParameterNames = New HashSet(Of String)
      For Each parameterSyntax In syntax.Parameters
        Dim parameterName = parameterSyntax.Identifier.Text
        Dim parameterType = BindTypeClause(parameterSyntax.Type)
        If Not seenParameterNames.Add(parameterName) Then
          m_diagnostics.ReportParameterAlreadyDeclared(parameterSyntax.Location, parameterName)
        Else
          Dim parameter = New ParameterSymbol(parameterName, parameterType, parameters.Count)
          parameters.Add(parameter)
        End If
      Next
      Dim type = If(BindTypeClause(syntax.Type), TypeSymbol.Void)
      Dim func = New FunctionSymbol(syntax.Identifier.Text, parameters.ToImmutable, type, syntax)
      'If Not m_scope.TryDeclareFunction(func) Then
      'If (func.Declaration.Identifier.Text IsNot Nothing AndAlso
      If (syntax.Identifier.Text IsNot Nothing AndAlso
          Not m_scope.TryDeclareFunction(func)) Then
        m_diagnostics.ReportSymbolAlreadyDeclared(syntax.Identifier.Location, func.Name)
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

    Private Function BindExpression(syntax As ExpressionSyntax, targetType As TypeSymbol) As BoundExpression
      Return BindConversion(syntax, targetType)
    End Function

    Public Function BindExpression(syntax As ExpressionSyntax, Optional canBeVoid As Boolean = False) As BoundExpression
      Dim result = BindExpressionInternal(syntax)
      If Not canBeVoid AndAlso result.Type Is TypeSymbol.Void Then
        m_diagnostics.ReportExpressionMustHaveValue(syntax.Location)
        Return New BoundErrorExpression
      End If
      Return result
    End Function

    Public Function BindExpressionInternal(syntax As ExpressionSyntax) As BoundExpression

      Select Case syntax.Kind
        Case SyntaxKind.ParenExpression
          Return BindParenExpression(DirectCast(syntax, ParenExpressionSyntax))
        Case SyntaxKind.LiteralExpression
          Return BindLiteralExpression(DirectCast(syntax, LiteralExpressionSyntax))
        Case SyntaxKind.NameExpression
          Return BindNameExpression(DirectCast(syntax, NameExpressionSyntax))
        Case SyntaxKind.AssignmentExpression
          Return BindAssignmentExpression(DirectCast(syntax, AssignmentExpressionSyntax))
        Case SyntaxKind.UnaryExpression
          Return BindUnaryEpression(DirectCast(syntax, UnaryExpressionSyntax))
        Case SyntaxKind.BinaryExpression
          Return BindBinaryEpression(DirectCast(syntax, BinaryExpressionSyntax))
        Case SyntaxKind.CallExpression
          Return BindCallExpression(DirectCast(syntax, CallExpressionSyntax))
        Case Else
          Throw New Exception($"Unexpected syntax {syntax.Kind}")
      End Select

    End Function

    Private Function BindErrorStatement() As BoundStatement
      Return New BoundExpressionStatement(New BoundErrorExpression)
    End Function

    Private Function BindGlobalStatement(syntax As StatementSyntax) As BoundStatement
      Return BindStatement(syntax, True)
    End Function

    Private Function BindStatement(syntax As StatementSyntax, Optional isGlobal As Boolean = False) As BoundStatement
      Dim result = BindStatementInternal(syntax)
      If Not m_isScript Or Not isGlobal Then
        If TypeOf result Is BoundExpressionStatement Then
          Dim es = CType(result, BoundExpressionStatement)
          Dim isAllowedExpression = es.Expression.Kind = BoundNodeKind.ErrorExpression Or
                                    es.Expression.Kind = BoundNodeKind.AssignmentExpression Or
                                    es.Expression.Kind = BoundNodeKind.CallExpression
          If Not isAllowedExpression Then
            m_diagnostics.ReportInvalidExpressionStatement(syntax.Location)
          End If
        End If
      End If
      Return result
    End Function

    Private Function BindStatementInternal(syntax As StatementSyntax) As BoundStatement

      Select Case syntax.Kind
        Case SyntaxKind.BlockStatement
          Return BindBlockStatement(DirectCast(syntax, BlockStatementSyntax))
        Case SyntaxKind.VariableDeclaration
          Return BindVariableDeclaration(DirectCast(syntax, VariableDeclarationSyntax))
        Case SyntaxKind.IfStatement
          Return BindIfStatement(DirectCast(syntax, IfStatementSyntax))
        Case SyntaxKind.WhileStatement
          Return BindWhileStatement(DirectCast(syntax, WhileStatementSyntax))
        Case SyntaxKind.DoWhileStatement
          Return BindDoWhileStatement(DirectCast(syntax, DoWhileStatementSyntax))
        Case SyntaxKind.ForStatement
          Return BindForStatement(DirectCast(syntax, ForStatementSyntax))
        Case SyntaxKind.BreakStatement
          Return BindBreakStatement(DirectCast(syntax, BreakStatementSyntax))
        Case SyntaxKind.ContinueStatement
          Return BindContinueStatement(DirectCast(syntax, ContinueStatementSyntax))
        Case SyntaxKind.ReturnStatement
          Return BindReturnStatement(DirectCast(syntax, ReturnStatementSyntax))
        Case SyntaxKind.ExpressionStatement
          Return BindExpressionStatement(DirectCast(syntax, ExpressionStatementSyntax))
        Case Else
          Throw New Exception($"Unexpected syntax {syntax.Kind}")
      End Select

    End Function

    Private Function BindBlockStatement(syntax As BlockStatementSyntax) As BoundStatement
      Dim statements = ImmutableArray.CreateBuilder(Of BoundStatement)
      m_scope = New BoundScope(m_scope)
      For Each statementSyntax In syntax.Statements
        Dim statement = BindStatement(statementSyntax)
        statements.Add(statement)
      Next
      m_scope = m_scope.Parent
      Return New BoundBlockStatement(statements.ToImmutable)
    End Function

    Private Function BindVariableDeclaration(syntax As VariableDeclarationSyntax) As BoundStatement
      Dim isReadOnly = syntax.Keyword.Kind = SyntaxKind.LetKeyword
      Dim type = BindTypeClause(syntax.TypeClause)
      Dim initializer = BindExpression(syntax.Initializer)
      Dim variableType = If(type, initializer.Type)
      Dim variable = BindVariableDeclaration(syntax.Identifier, isReadOnly, variableType, initializer.ConstantValue)
      Dim convertedInitializer = BindConversion(syntax.Initializer.Location, initializer, variableType)
      Return New BoundVariableDeclaration(variable, convertedInitializer)
    End Function

    Private Function BindTypeClause(syntax As TypeClauseSyntax) As TypeSymbol
      If syntax Is Nothing Then
        Return Nothing
      End If
      Dim type = LookupType(syntax.Identifier.Text)
      If type Is Nothing Then
        m_diagnostics.ReportUndefinedType(syntax.Identifier.Location, syntax.Identifier.Text)
        Return Nothing
      End If
      Return type
    End Function

    Private Function BindIfStatement(syntax As IfStatementSyntax) As BoundStatement
      Dim condition = BindExpression(syntax.Condition, TypeSymbol.Bool)

      If condition.ConstantValue IsNot Nothing Then
        If Not CBool(condition.ConstantValue.Value) Then
          m_diagnostics.ReportUnreachableCode(syntax.ThenStatement)
        ElseIf syntax.ElseClause IsNot Nothing Then
          m_diagnostics.ReportUnreachableCode(syntax.ElseClause.ElseStatement)
        End If
      End If

      Dim thenStatement = BindStatement(syntax.ThenStatement)
      Dim elseStatement = If(syntax.ElseClause IsNot Nothing, BindStatement(syntax.ElseClause.ElseStatement), Nothing)
      Return New BoundIfStatement(condition, thenStatement, elseStatement)
    End Function

    Private Function BindWhileStatement(syntax As WhileStatementSyntax) As BoundStatement
      Dim condition = BindExpression(syntax.Condition, TypeSymbol.Bool)

      If condition.ConstantValue IsNot Nothing Then
        If Not CBool(condition.ConstantValue.Value) Then
          m_diagnostics.ReportUnreachableCode(syntax.Body)
        End If
      End If

      Dim breakLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim body = BindLoopBody(syntax.Body, breakLabel, continueLabel)
      Return New BoundWhileStatement(condition, body, breakLabel, continueLabel)
    End Function

    Private Function BindDoWhileStatement(syntax As DoWhileStatementSyntax) As BoundStatement
      Dim breakLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim body = BindLoopBody(syntax.Body, breakLabel, continueLabel)
      Dim condition = BindExpression(syntax.Condition, TypeSymbol.Bool)
      Return New BoundDoWhileStatement(body, condition, breakLabel, continueLabel)
    End Function

    Private Function BindForStatement(syntax As ForStatementSyntax) As BoundStatement

      Dim lowerBound = BindExpression(syntax.LowerBound, TypeSymbol.Int)
      Dim upperBound = BindExpression(syntax.UpperBound, TypeSymbol.Int)

      m_scope = New BoundScope(m_scope)

      'Dim variable = BindVariable(syntax.Identifier, True, TypeSymbol.Int)
      Dim variable = BindVariableDeclaration(syntax.Identifier, isReadOnly:=True, TypeSymbol.Int)
      Dim breakLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim body = BindLoopBody(syntax.Body, breakLabel, continueLabel)

      m_scope = m_scope.Parent

      Return New BoundForStatement(variable, lowerBound, upperBound, body, breakLabel, continueLabel)

    End Function

    Private Function BindLoopBody(body As StatementSyntax, ByRef breakLabel As BoundLabel, ByRef continueLabel As BoundLabel) As BoundStatement

      m_labelCounter += 1
      breakLabel = New BoundLabel($"break{m_labelCounter}")
      continueLabel = New BoundLabel($"continue{m_labelCounter}")

      m_loopStack.Push((breakLabel, continueLabel))
      Dim boundBody = BindStatement(body)
      m_loopStack.Pop()

      Return boundBody

    End Function

    Private Function BindBreakStatement(syntax As BreakStatementSyntax) As BoundStatement

      If m_loopStack.Count = 0 Then
        m_diagnostics.ReportInvalidBreakOrContinue(syntax.Keyword.Location, syntax.Keyword.Text)
        Return BindErrorStatement()
      End If

      Dim breakLabel = m_loopStack.Peek().BreakLabel
      Return New BoundGotoStatement(breakLabel)

    End Function

    Private Function BindContinueStatement(syntax As ContinueStatementSyntax) As BoundStatement
      If m_loopStack.Count = 0 Then
        m_diagnostics.ReportInvalidBreakOrContinue(syntax.Keyword.Location, syntax.Keyword.Text)
        Return BindErrorStatement()
      End If
      Dim continueLabel = m_loopStack.Peek().ContinueLabel
      Return New BoundGotoStatement(continueLabel)
    End Function

    Private Function BindReturnStatement(syntax As ReturnStatementSyntax) As BoundStatement

      Dim expression = If(syntax.Expression Is Nothing, Nothing, BindExpression(syntax.Expression))

      If m_function Is Nothing Then
        If m_isScript Then
          ' Ignore because we allow both return with and without values.
          If expression Is Nothing Then
            expression = New BoundLiteralExpression("")
          End If
        ElseIf expression IsNot Nothing Then
          ' Main does not support return values.
          m_diagnostics.ReportInvalidReturnWithValueInGlobalStatements(syntax.Expression.Location)
        End If
      Else
        If m_function.Type Is TypeSymbol.Void Then
          If expression IsNot Nothing Then
            m_diagnostics.ReportInvalidReturnExpression(syntax.Expression.Location, m_function.Name)
          End If
        Else
          If expression Is Nothing Then
            m_diagnostics.ReportMissingReturnExpression(syntax.ReturnKeyword.Location, m_function.Type)
          Else
            expression = BindConversion(syntax.Expression.Location, expression, m_function.Type)
          End If
        End If
      End If

      Return New BoundReturnStatement(expression)

    End Function

    Private Function BindExpressionStatement(syntax As ExpressionStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression, True)
      Return New BoundExpressionStatement(expression)
    End Function

    Private Function BindParenExpression(syntax As ParenExpressionSyntax) As BoundExpression
      Return BindExpression(syntax.Expression)
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
      'Dim variable As VariableSymbol = Nothing
      'If Not m_scope.TryLookupVariable(name, variable) Then
      '  Diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name)
      Dim variable = BindVariableReference(syntax.IdentifierToken)
      If variable Is Nothing Then
        Return New BoundErrorExpression
      End If
      'End If
      Return New BoundVariableExpression(variable)
    End Function

    Private Function BindAssignmentExpression(syntax As AssignmentExpressionSyntax) As BoundExpression

      Dim name = syntax.IdentifierToken.Text
      Dim boundExpression = BindExpression(syntax.Expression)

      'Dim variable As VariableSymbol = Nothing
      'If Not m_scope.TryLookupVariable(name.ToLower, variable) Then
      '  Diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name)
      Dim variable = BindVariableReference(syntax.IdentifierToken)
      If variable Is Nothing Then
        Return boundExpression
      End If
      'End If

      If variable.IsReadOnly Then
        m_diagnostics.ReportCannotAssign(syntax.EqualsToken.Location, name)
      End If

      Dim convertedExpression = BindConversion(syntax.Expression.Location, boundExpression, variable.Type)

      Return New BoundAssignmentExpression(variable, convertedExpression)

    End Function

    Private Function BindUnaryEpression(syntax As UnaryExpressionSyntax) As BoundExpression
      Dim boundOperand = BindExpression(syntax.Operand)
      If boundOperand.Type Is TypeSymbol.Error Then
        Return New BoundErrorExpression
      End If
      Dim boundOperator = BoundUnaryOperator.Bind(syntax.OperatorToken.Kind, boundOperand.Type)
      If boundOperator Is Nothing Then
        m_diagnostics.ReportUndefinedUnaryOperator(syntax.OperatorToken.Location, syntax.OperatorToken.Text, boundOperand.Type)
        Return New BoundErrorExpression
      End If
      Return New BoundUnaryExpression(boundOperator, boundOperand)
    End Function

    Private Function BindBinaryEpression(syntax As BinaryExpressionSyntax) As BoundExpression
      Dim boundLeft = BindExpression(syntax.Left)
      Dim boundRight = BindExpression(syntax.Right)
      If boundLeft.Type Is TypeSymbol.Error OrElse boundRight.Type Is TypeSymbol.Error Then
        Return New BoundErrorExpression
      End If
      Dim boundOperator = BoundBinaryOperator.Bind(syntax.OperatorToken.Kind, boundLeft.Type, boundRight.Type)
      If boundOperator Is Nothing Then
        m_diagnostics.ReportUndefinedBinaryOperator(syntax.OperatorToken.Location, syntax.OperatorToken.Text, boundLeft.Type, boundRight.Type)
        Return New BoundErrorExpression
      End If
      Return New BoundBinaryExpression(boundLeft, boundOperator, boundRight)
    End Function

    Private Function BindCallExpression(syntax As CallExpressionSyntax) As BoundExpression

      Dim t = LookupType(syntax.Identifier.Text)
      If syntax.Arguments.Count = 1 AndAlso TypeOf t Is TypeSymbol Then
        Return BindConversion(syntax.Arguments(0), t, True)
      End If

      Dim boundArguments = ImmutableArray.CreateBuilder(Of BoundExpression)

      For Each argument In syntax.Arguments
        Dim boundArgument = BindExpression(argument)
        boundArguments.Add(boundArgument)
      Next

      'Dim func As FunctionSymbol = Nothing

      'If Not m_scope.TryLookupFunction(syntax.Identifier.Text, func) Then
      Dim symbol = m_scope.TryLookupSymbol(syntax.Identifier.Text)
      If symbol Is Nothing Then
        m_diagnostics.ReportUndefinedFunction(syntax.Identifier.Location, syntax.Identifier.Text)
        Return New BoundErrorExpression
      End If
      Dim func = TryCast(symbol, FunctionSymbol)
      If func Is Nothing Then
        m_diagnostics.ReportNotAFunction(syntax.Identifier.Location, syntax.Identifier.Text)
        Return New BoundErrorExpression
      End If
      If syntax.Arguments.Count <> func.Parameters.Length Then
        'Me.Diagnostics.ReportWrongArgumentCount(syntax.Span, func.Name, func.Parameters.Length, syntax.Arguments.Count)
        Dim span As TextSpan
        If syntax.Arguments.Count > func.Parameters.Length Then
          Dim firstExceedingNode As SyntaxNode
          If func.Parameters.Length > 0 Then
            firstExceedingNode = syntax.Arguments.GetSeparator(func.Parameters.Length - 1)
          Else
            firstExceedingNode = syntax.Arguments(0)
          End If
          Dim lastExceedingArgument = syntax.Arguments(syntax.Arguments.Count - 1)
          span = TextSpan.FromBounds(firstExceedingNode.Span.Start, lastExceedingArgument.Span.End)
        Else
          span = syntax.CloseParen.Span
        End If
        Dim location = New TextLocation(syntax.SyntaxTree.Text, span)
        m_diagnostics.ReportWrongArgumentCount(location, func.Name, func.Parameters.Length, syntax.Arguments.Count)
        Return New BoundErrorExpression
      End If

      'Dim hasErrors = False
      For i = 0 To syntax.Arguments.Count - 1

        Dim argumentLocation = syntax.Arguments(i).Location
        Dim argument = boundArguments(i)
        Dim parameter = func.Parameters(i)

        boundArguments(i) = BindConversion(argumentLocation, argument, parameter.Type)

        'Dim conv = Conversion.Classify(argument.Type, parameter.Type)
        'If Not conv.IsImplicit Then

        'End If

        'If argument.Type IsNot parameter.Type Then
        '  If argument.Type IsNot TypeSymbol.Error Then
        '    m_diagnostics.ReportWrongArgumentType(syntax.Arguments(i).Location, parameter.Name, parameter.Type, argument.Type)
        '  End If
        '  hasErrors = True
        'End If
      Next
      'If hasErrors Then
      '  Return New BoundErrorExpression
      'End If

      Return New BoundCallExpression(func, boundArguments.ToImmutableArray)

    End Function

    Private Function BindConversion(syntax As ExpressionSyntax, [type] As TypeSymbol, Optional allowExplicit As Boolean = False) As BoundExpression
      Dim expression = BindExpression(syntax)
      Return BindConversion(syntax.Location, expression, type, allowExplicit)
    End Function

    Private Function BindConversion(diagnosticLocation As TextLocation,
                                    expression As BoundExpression,
                                    type As TypeSymbol,
                                    Optional allowExplicit As Boolean = False) As BoundExpression
      Dim c = Conversion.Classify(expression.Type, [type])
      If Not c.Exists Then
        If expression.Type IsNot TypeSymbol.Error AndAlso [type] IsNot TypeSymbol.Error Then
          m_diagnostics.ReportCannotConvert(diagnosticLocation, expression.Type, [type])
        End If
        Return New BoundErrorExpression
      End If
      If Not allowExplicit AndAlso c.IsExplicit Then
        m_diagnostics.ReportCannotConvertImplicitly(diagnosticLocation, expression.Type, [type])
      End If

      If c.IsIdentity Then Return expression
      Return New BoundConversionExpression([type], expression)
    End Function

    'Private Function BindVariable(identifier As SyntaxToken, isReadOnly As Boolean, type As TypeSymbol) As VariableSymbol
    Private Function BindVariableDeclaration(identifier As SyntaxToken, isReadOnly As Boolean, type As TypeSymbol, Optional constant As BoundConstant = Nothing) As VariableSymbol
      Dim name = If(identifier.Text, "?")
      Dim [declare] = Not identifier.IsMissing
      Dim variable = If(m_function Is Nothing,
                                    DirectCast(New GlobalVariableSymbol(name, isReadOnly, type, constant), VariableSymbol),
                                    DirectCast(New LocalVariableSymbol(name, isReadOnly, type, constant), VariableSymbol))
      If [declare] AndAlso Not m_scope.TryDeclareVariable(variable) Then
        m_diagnostics.ReportSymbolAlreadyDeclared(identifier.Location, name)
      End If
      Return variable
    End Function

    Private Function BindVariableReference(ByVal identifierToken As SyntaxToken) As VariableSymbol

      Dim name = identifierToken.Text
      Dim s = m_scope.TryLookupSymbol(name)

      If TypeOf s Is VariableSymbol Then
        Return TryCast(s, VariableSymbol)
      ElseIf s Is Nothing Then
        m_diagnostics.ReportUndefinedVariable(identifierToken.Location, name)
        Return Nothing
      Else
        m_diagnostics.ReportNotAVariable(identifierToken.Location, name)
        Return Nothing
      End If

    End Function


    Private Function LookupType(name As String) As TypeSymbol
      Select Case name
#If BASIC Then
        Case "INTEGER" : Return TypeSymbol.Int
        Case "LONG" : Return TypeSymbol.Int
        Case "SINGLE" : Return TypeSymbol.Int
        Case "DOUBLE" : Return TypeSymbol.Int
        Case "STRING" : Return TypeSymbol.String
#End If
        Case "any" : Return TypeSymbol.Any
        Case "bool" : Return TypeSymbol.Bool
        Case "int" : Return TypeSymbol.Int
        Case "string" : Return TypeSymbol.String
        Case Else
          Return Nothing
      End Select
    End Function

  End Class

End Namespace