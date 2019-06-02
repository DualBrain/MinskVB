Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis

  Public NotInheritable Class Evaluator

    Sub New(root As ExpressionSyntax)
      Me.Root = root
    End Sub

    Public ReadOnly Property Root As ExpressionSyntax

    Public Function Evaluate() As Integer
      Return Me.EvaluateExpression(Me.Root)
    End Function

    Private Function EvaluateExpression(node As ExpressionSyntax) As Integer

      ' NumberExpression
      ' BinaryExpression
      ' ParenExpression

      If TypeOf node Is LiteralExpressionSyntax Then
        Return CInt(DirectCast(node, LiteralExpressionSyntax).LiteralToken.Value)
      End If

      If TypeOf node Is UnaryExpressionSyntax Then
        Dim u = DirectCast(node, UnaryExpressionSyntax)
        Dim operand = Me.EvaluateExpression(u.Operand)
        If u.OperatorToken.Kind = SyntaxKind.PlusToken Then
          Return operand
        ElseIf u.OperatorToken.Kind = SyntaxKind.MinusToken Then
          Return -operand
        Else
          Throw New Exception($"Unexpected unary operator {u.OperatorToken.Kind}")
        End If
      End If

      If TypeOf node Is BinaryExpressionSyntax Then
        Dim b = DirectCast(node, BinaryExpressionSyntax)
        Dim left = Me.EvaluateExpression(b.Left)
        Dim right = Me.EvaluateExpression(b.Right)
        Select Case b.OperatorToken.Kind
          Case SyntaxKind.PlusToken : Return left + right
          Case SyntaxKind.MinusToken : Return left - right
          Case SyntaxKind.StarToken : Return left * right
          Case SyntaxKind.SlashToken : Return left \ right
          Case Else
            Throw New Exception($"Unexpected binary operator {b.OperatorToken.Kind}")
        End Select
      End If

      If TypeOf node Is ParenExpressionSyntax Then
        Dim p = DirectCast(node, ParenExpressionSyntax)
        Return Me.EvaluateExpression(p.Expression)
      End If

      Throw New Exception($"Unexpected node {node.Kind}")

    End Function

  End Class

End Namespace