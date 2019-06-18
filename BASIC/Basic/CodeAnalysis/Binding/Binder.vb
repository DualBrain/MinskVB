Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundGlobalScope

    Sub New(previous As BoundGlobalScope, diagnostics As ImmutableArray(Of Diagnostic), variables As ImmutableArray(Of VariableSymbol), statement As BoundStatement)
      Me.Previous = previous
      Me.Diagnostics = diagnostics
      Me.Variables = variables
      Me.Statement = statement
    End Sub

    Public ReadOnly Property Previous As BoundGlobalScope
    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Variables As ImmutableArray(Of VariableSymbol)
    Public ReadOnly Property Statement As BoundStatement

  End Class

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

      ' submission 3 -> submission 2 -> submission 1

      Dim parent As BoundScope = Nothing

      While stack.Count > 0
        previous = stack.Pop
        Dim scope = New BoundScope(parent)
        For Each v In previous.Variables
          scope.TryDeclare(v)
        Next
        parent = scope
      End While

      Return parent

    End Function

    Public ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag

    Private Function BindExpression(syntax As ExpressionSyntax, targetType As TypeSymbol) As BoundExpression
      Dim result = Me.BindExpression(syntax)
      If targetType IsNot TypeSymbol.Error AndAlso
         result.Type IsNot TypeSymbol.Error AndAlso
         result.Type IsNot targetType Then
        Me.Diagnostics.ReportCannotConvert(syntax.Span, result.Type, targetType)
      End If
      Return result
    End Function

    Public Function BindExpression(syntax As ExpressionSyntax) As BoundExpression

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
      Dim expression = Me.BindExpression(syntax.Expression)
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
      If Not Me.m_scope.TryLookup(name, variable) Then
        Me.Diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name)
        Return New BoundErrorExpression
      End If
      Return New BoundVariableExpression(variable)
    End Function

    Private Function BindAssignmentExpression(syntax As AssignmentExpressionSyntax) As BoundExpression

      Dim name = syntax.IdentifierToken.Text
      Dim boundExpression = Me.BindExpression(syntax.Expression)

      Dim variable As VariableSymbol = Nothing
      If Not Me.m_scope.TryLookup(name.ToLower, variable) Then
        Me.Diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name)
        Return boundExpression
      End If

      If variable.IsReadOnly Then
        Me.Diagnostics.ReportCannotAssign(syntax.EqualsToken.Span, name)
      End If

      If boundExpression.Type IsNot variable.Type Then
        Me.Diagnostics.ReportCannotConvert(syntax.Expression.Span, boundExpression.Type, variable.Type)
        Return boundExpression
      End If

      Return New BoundAssignmentExpression(variable, boundExpression)

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

    Private Function BindVariable(identifier As SyntaxToken, isReadOnly As Boolean, type As TypeSymbol) As VariableSymbol
      Dim name = If(identifier.Text, "?")
      Dim [declare] = Not identifier.IsMissing
      Dim variable = New VariableSymbol(name, isReadOnly, type)
      If [declare] AndAlso Not Me.m_scope.TryDeclare(variable) Then
        Me.Diagnostics.ReportVariableAlreadyDeclared(identifier.Span, name)
      End If
      Return variable
    End Function

  End Class

End Namespace