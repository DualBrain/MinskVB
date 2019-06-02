Option Explicit On
Option Strict On
Option Infer On
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class Binder

    Private ReadOnly m_diagnostics As List(Of String) = New List(Of String)

    Public ReadOnly Property Diagnostics As IEnumerable(Of String)
      Get
        Return Me.m_diagnostics
      End Get
    End Property

    Public Function BindExpression(syntax As ExpressionSyntax) As BoundExpression

      Select Case syntax.Kind
        Case SyntaxKind.LiteralExpression
          Return Me.BindLiteralEpression(DirectCast(syntax, LiteralExpressionSyntax))
        Case SyntaxKind.UnaryExpression
          Return Me.BindUnaryEpression(DirectCast(syntax, UnaryExpressionSyntax))
        Case SyntaxKind.BinaryExpression
          Return Me.BindBinaryEpression(DirectCast(syntax, BinaryExpressionSyntax))
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
      Dim boundOperatorKind = Me.BindUnaryOperatorKind(syntax.OperatorToken.Kind, boundOperand.Type)
      If boundOperatorKind Is Nothing Then
        Me.m_diagnostics.Add($"Unary operator '{syntax.OperatorToken.Text}' is not defined for type {boundOperand.Type}.")
        Return boundOperand
      End If
      Return New BoundUnaryExpression(boundOperatorKind.Value, boundOperand)
    End Function

    Private Function BindBinaryEpression(syntax As BinaryExpressionSyntax) As BoundExpression
      Dim boundLeft = Me.BindExpression(syntax.Left)
      Dim boundRight = Me.BindExpression(syntax.Right)
      Dim boundOperatorKind = Me.BindBinaryOperatorKind(syntax.OperatorToken.Kind, boundLeft.Type, boundRight.Type)
      If boundOperatorKind Is Nothing Then
        Me.m_diagnostics.Add($"Binary operator '{syntax.OperatorToken.Text}' is not defined for type {boundLeft.Type} and {boundRight.Type}.")
        Return boundLeft
      End If
      Return New BoundBinaryExpression(boundLeft, boundOperatorKind.Value, boundRight)
    End Function

    Private Function BindUnaryOperatorKind(kind As SyntaxKind, operandType As Type) As BoundUnaryOperatorKind?

      If operandType Is GetType(Integer) Then
        Select Case kind
          Case SyntaxKind.PlusToken
            Return BoundUnaryOperatorKind.Identity
          Case SyntaxKind.MinusToken
            Return BoundUnaryOperatorKind.Negation
          Case Else
        End Select
      End If

      If operandType Is GetType(Boolean) Then
        Select Case kind
          Case SyntaxKind.BangToken, SyntaxKind.NotKeyword
            Return BoundUnaryOperatorKind.LogicalNegation
          Case Else
        End Select
      End If

      Return Nothing

    End Function

    Private Function BindBinaryOperatorKind(kind As SyntaxKind, leftType As Type, rightType As Type) As BoundBinaryOperatorKind?

      If leftType Is GetType(Integer) AndAlso
         rightType Is GetType(Integer) Then

        Select Case kind
          Case SyntaxKind.PlusToken : Return BoundBinaryOperatorKind.Addition
          Case SyntaxKind.MinusToken : Return BoundBinaryOperatorKind.Subtraction
          Case SyntaxKind.StarToken : Return BoundBinaryOperatorKind.Multiplication
          Case SyntaxKind.SlashToken : Return BoundBinaryOperatorKind.Division
          Case Else
        End Select

      End If

      If leftType Is GetType(Boolean) AndAlso
         rightType Is GetType(Boolean) Then

        Select Case kind
          Case SyntaxKind.AmpersandAmpersandToken, SyntaxKind.AndKeyword : Return BoundBinaryOperatorKind.LogicalAnd
          Case SyntaxKind.PipePipeToken, SyntaxKind.OrKeyword : Return BoundBinaryOperatorKind.LogicalOr
          Case Else
        End Select

      End If

      Return Nothing

    End Function

  End Class

End Namespace