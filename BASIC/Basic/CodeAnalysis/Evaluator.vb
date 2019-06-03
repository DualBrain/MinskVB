Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Binding

Namespace Global.Basic.CodeAnalysis

  Friend NotInheritable Class Evaluator

    Sub New(root As BoundExpression)
      Me.Root = root
    End Sub

    Public ReadOnly Property Root As BoundExpression

    Public Function Evaluate() As Object
      Return Me.EvaluateExpression(Me.Root)
    End Function

    Private Function EvaluateExpression(node As BoundExpression) As Object

      ' NumberExpression
      ' BinaryExpression
      ' ParenExpression

      If TypeOf node Is BoundLiteralExpression Then
        Return DirectCast(node, BoundLiteralExpression).Value
      End If

      If TypeOf node Is BoundUnaryExpression Then
        Dim u = DirectCast(node, BoundUnaryExpression)
        Dim operand = Me.EvaluateExpression(u.Operand)
        Select Case u.Op.Kind
          Case BoundUnaryOperatorKind.Identity
            Return CInt(operand)
          Case BoundUnaryOperatorKind.Negation
            Return -CInt(operand)
          Case BoundUnaryOperatorKind.LogicalNegation
            Return Not CBool(operand)
          Case Else
            Throw New Exception($"Unexpected unary operator {u.Op}")
        End Select
      End If

      If TypeOf node Is BoundBinaryExpression Then
        Dim b = DirectCast(node, BoundBinaryExpression)
        Dim left = Me.EvaluateExpression(b.Left)
        Dim right = Me.EvaluateExpression(b.Right)
        Select Case b.Op.Kind
          Case BoundBinaryOperatorKind.Addition : Return CInt(left) + CInt(right)
          Case BoundBinaryOperatorKind.Subtraction : Return CInt(left) - CInt(right)
          Case BoundBinaryOperatorKind.Multiplication : Return CInt(left) * CInt(right)
          Case BoundBinaryOperatorKind.Division : Return CInt(left) \ CInt(right)
          Case BoundBinaryOperatorKind.LogicalAnd : Return CBool(left) And CBool(right)
          Case BoundBinaryOperatorKind.LogicalOr : Return CBool(left) Or CBool(right)
          Case BoundBinaryOperatorKind.Equals : Return Equals(left, right)
          Case BoundBinaryOperatorKind.NotEquals : Return Not Equals(left, right)
          Case Else
            Throw New Exception($"Unexpected binary operator {b.Op}")
        End Select
      End If

      Throw New Exception($"Unexpected node {node.Kind}")

    End Function

  End Class

End Namespace