Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Binding

Namespace Global.Basic.CodeAnalysis

  Friend NotInheritable Class Evaluator

    Sub New(root As BoundExpression, variables As Dictionary(Of VariableSymbol, Object))
      Me.Root = root
      Me.Variables = variables
    End Sub

    Public ReadOnly Property Root As BoundExpression
    Public ReadOnly Property Variables As Dictionary(Of VariableSymbol, Object)

    Public Function Evaluate() As Object
      Return Me.EvaluateExpression(Me.Root)
    End Function

    Private Function EvaluateExpression(node As BoundExpression) As Object

      Select Case node.Kind
        Case BoundNodeKind.LiteralExpression : Return Me.EvaluateLiteralExpression(node)
        Case BoundNodeKind.VariableExpression : Return Me.EvaluateVariableExpression(node)
        Case BoundNodeKind.AssignmentExpression : Return Me.EvaluateAssignmentExpression(node)
        Case BoundNodeKind.UnaryExpression : Return Me.EvaluateUnaryExpression(node)
        Case BoundNodeKind.BinaryExpression : Return Me.EvaluateBinaryExpression(node)
        Case Else
          Throw New Exception($"Unexpected node {node.Kind}")
      End Select

    End Function

    Private Function EvaluateLiteralExpression(node As BoundExpression) As Object
      Return DirectCast(node, BoundLiteralExpression).Value
    End Function

    Private Function EvaluateVariableExpression(node As BoundExpression) As Object
      Dim v = DirectCast(node, BoundVariableExpression)
      Return Me.Variables(v.Variable)
    End Function

    Private Function EvaluateAssignmentExpression(node As BoundExpression) As Object
      Dim a = DirectCast(node, BoundAssignmentExpression)
      Dim value = Me.EvaluateExpression(a.Expression)
      Me.Variables(a.Variable) = value
      Return value
    End Function

    Private Function EvaluateUnaryExpression(node As BoundExpression) As Object
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
    End Function

    Private Function EvaluateBinaryExpression(node As BoundExpression) As Object
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
    End Function

  End Class

End Namespace