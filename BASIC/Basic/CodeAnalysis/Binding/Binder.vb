Option Explicit On
Option Strict On
Option Infer On
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class Binder

    Public ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag

    Public Function BindExpression(syntax As ExpressionSyntax) As BoundExpression

      Select Case syntax.Kind
        Case SyntaxKind.LiteralExpression
          Return Me.BindLiteralEpression(DirectCast(syntax, LiteralExpressionSyntax))
        Case SyntaxKind.UnaryExpression
          Return Me.BindUnaryEpression(DirectCast(syntax, UnaryExpressionSyntax))
        Case SyntaxKind.BinaryExpression
          Return Me.BindBinaryEpression(DirectCast(syntax, BinaryExpressionSyntax))
        Case SyntaxKind.ParenExpression
          Return Me.BindExpression(DirectCast(syntax, ParenExpressionSyntax).Expression)
        Case Else
          Throw New Exception($"Unexpected syntax {syntax.Kind}")
      End Select

    End Function

    Private Function BindLiteralEpression(syntax As LiteralExpressionSyntax) As BoundExpression
      Dim value = If(syntax.Value, 0)
      Return New BoundLiteralExpression(value)
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