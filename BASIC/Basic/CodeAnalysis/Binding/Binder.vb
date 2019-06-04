Option Explicit On
Option Strict On
Option Infer On
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class Binder

    Private ReadOnly m_variables As Dictionary(Of VariableSymbol, Object)

    Public Sub New(variables As Dictionary(Of VariableSymbol, Object))
      Me.m_variables = variables
    End Sub

    Public ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag

    Public Function BindExpression(syntax As ExpressionSyntax) As BoundExpression

      Select Case syntax.Kind
        Case SyntaxKind.ParenExpression
          Return Me.BindParenExpression(DirectCast(syntax, ParenExpressionSyntax))
        Case SyntaxKind.LiteralExpression
          Return Me.BindLiteralEpression(DirectCast(syntax, LiteralExpressionSyntax))
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

    Private Function BindParenExpression(syntax As ParenExpressionSyntax) As BoundExpression
      Return Me.BindExpression(syntax.Expression)
    End Function

    Private Function BindLiteralEpression(syntax As LiteralExpressionSyntax) As BoundExpression
      Dim value = If(syntax.Value, 0)
      Return New BoundLiteralExpression(value)
    End Function

    Private Function BindNameExpression(syntax As NameExpressionSyntax) As BoundExpression
      Dim name = syntax.IdentifierToken.Text
      Dim variable = Me.m_variables.Keys.FirstOrDefault(Function(v) v.Name = name.ToLower)
      'Dim value As Object = Nothing
      'If Not Me.m_variables.TryGetValue(name.ToLower, value) Then
      If variable Is Nothing Then
        Me.Diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name)
        Return New BoundLiteralExpression(0)
      End If
      'Dim type = value.GetType()
      Return New BoundVariableExpression(variable)
    End Function

    Private Function BindAssignmentExpression(syntax As AssignmentExpressionSyntax) As BoundExpression
      Dim name = syntax.IdentifierToken.Text
      Dim boundExpression = Me.BindExpression(syntax.Expression)

      ' We are here: 1:37:30

      Dim existingVariable = Me.m_variables.Keys.FirstOrDefault(Function(v) v.Name = name.ToLower)
      If existingVariable IsNot Nothing Then
        Me.m_variables.Remove(existingVariable)
      End If
      Dim variable = New VariableSymbol(name.ToLower, boundExpression.Type)
      Me.m_variables(variable) = Nothing

      'Dim defaultValue = If(boundExpression.Type = GetType(Integer), CObj(0), If(boundExpression.Type = GetType(Boolean), False, Nothing))

      'If defaultValue Is Nothing Then
      '  Throw New Exception($"Unsupported variable type: {boundExpression.Type}")
      'End If

      'Me.m_variables(name) = defaultValue

      Return New BoundAssignmentExpression(variable, boundExpression)
    End Function

    Private Function BindUnaryEpression(syntax As UnaryExpressionSyntax) As BoundExpression
      Dim boundOperand = Me.BindExpression(syntax.Operand)
      Dim boundOperator = BoundUnaryOperator.Bind(syntax.OperatorToken.Kind, boundOperand.Type)
      If boundOperator Is Nothing Then
        Me.Diagnostics.ReportUndefinedUnaryOperator(syntax.OperatorToken.Span, syntax.OperatorToken.Text, boundOperand.Type)
        Return boundOperand
      End If
      Return New BoundUnaryExpression(boundOperator, boundOperand)
    End Function

    Private Function BindBinaryEpression(syntax As BinaryExpressionSyntax) As BoundExpression
      Dim boundLeft = Me.BindExpression(syntax.Left)
      Dim boundRight = Me.BindExpression(syntax.Right)
      Dim boundOperator = BoundBinaryOperator.Bind(syntax.OperatorToken.Kind, boundLeft.Type, boundRight.Type)
      If boundOperator Is Nothing Then
        Me.Diagnostics.ReportUndefinedBinaryOperator(syntax.OperatorToken.Span, syntax.OperatorToken.Text, boundLeft.Type, boundRight.Type)
        Return boundLeft
      End If
      Return New BoundBinaryExpression(boundLeft, boundOperator, boundRight)
    End Function

  End Class

End Namespace