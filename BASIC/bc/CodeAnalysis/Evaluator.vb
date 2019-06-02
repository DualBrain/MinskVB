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
        Dim operand = CInt(Me.EvaluateExpression(u.Operand))
        Select Case u.OperatorKind
          Case BoundUnaryOperatorKind.Identity
            Return operand
          Case BoundUnaryOperatorKind.Negation
            Return -operand
          Case Else
            Throw New Exception($"Unexpected unary operator {u.OperatorKind}")
        End Select
      End If

      If TypeOf node Is BoundBinaryExpression Then
        Dim b = DirectCast(node, BoundBinaryExpression)
        Dim left = CInt(Me.EvaluateExpression(b.Left))
        Dim right = CInt(Me.EvaluateExpression(b.Right))
        Select Case b.OperatorKind
          Case BoundBinaryOperatorKind.Addition : Return left + right
          Case BoundBinaryOperatorKind.Subtraction : Return left - right
          Case BoundBinaryOperatorKind.Multiplication : Return left * right
          Case BoundBinaryOperatorKind.Division : Return left \ right
          Case Else
            Throw New Exception($"Unexpected binary operator {b.OperatorKind}")
        End Select
      End If

      Throw New Exception($"Unexpected node {node.Kind}")

    End Function

  End Class

End Namespace