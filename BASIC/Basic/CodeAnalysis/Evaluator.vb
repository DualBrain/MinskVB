Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Binding
Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis

  Friend NotInheritable Class Evaluator

    Private m_lastValue As Object

    Sub New(root As BoundBlockStatement, variables As Dictionary(Of VariableSymbol, Object))
      Me.Root = root
      Me.Variables = variables
    End Sub

    Public ReadOnly Property Root As BoundBlockStatement
    Public ReadOnly Property Variables As Dictionary(Of VariableSymbol, Object)

    Public Function Evaluate() As Object

      Dim labelToIndex = New Dictionary(Of BoundLabel, Integer)
      For i = 0 To Me.Root.Statements.Length - 1
        If TypeOf Me.Root.Statements(i) Is BoundLabelStatement Then
          labelToIndex.Add(DirectCast(Me.Root.Statements(i), BoundLabelStatement).Label, i + 1)
        End If
      Next

      Dim index = 0
      While index < Me.Root.Statements.Length
        Dim s = Me.Root.Statements(index)
        Select Case s.Kind
          Case BoundNodeKind.VariableDeclaration : Me.EvaluateVariableDeclaration(DirectCast(s, BoundVariableDeclaration)) : index += 1
          Case BoundNodeKind.ExpressionStatement : Me.EvaluateExpressionStatement(DirectCast(s, BoundExpressionStatement)) : index += 1
          Case BoundNodeKind.GotoStatement
            Dim gs As BoundGotoStatement = DirectCast(s, BoundGotoStatement)
            index = labelToIndex(gs.Label)
          Case BoundNodeKind.ConditionalGotoStatement
            Dim cgs = DirectCast(s, BoundConditionalGotoStatement)
            Dim condition = CBool(Me.EvaluateExpression(cgs.Condition))
            'If (condition AndAlso Not cgs.JumpIfFalse) OrElse
            '   (Not condition AndAlso cgs.JumpIfFalse) Then
            If condition = cgs.JumpIfTrue Then
              index = labelToIndex(cgs.Label)
            Else
              index += 1
            End If
          Case BoundNodeKind.LabelStatement : index += 1
          Case Else
            Throw New Exception($"Unexpected statement {s.Kind}")
        End Select
      End While

      Return Me.m_lastValue

    End Function

    Private Sub EvaluateVariableDeclaration(node As BoundVariableDeclaration)
      Dim value = Me.EvaluateExpression(node.Initializer)
      Me.Variables(node.Variable) = value
      Me.m_lastValue = value
    End Sub

    Private Sub EvaluateExpressionStatement(node As BoundExpressionStatement)
      Me.m_lastValue = Me.EvaluateExpression(node.Expression)
    End Sub

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
        Case BoundUnaryOperatorKind.Onescomplement
          Return Not CInt(operand)
        Case Else
          Throw New Exception($"Unexpected unary operator {u.Op}")
      End Select
    End Function

    Private Function EvaluateBinaryExpression(node As BoundExpression) As Object
      Dim b = DirectCast(node, BoundBinaryExpression)
      Dim left = Me.EvaluateExpression(b.Left)
      Dim right = Me.EvaluateExpression(b.Right)
      Select Case b.Op.Kind
        Case BoundBinaryOperatorKind.Addition
          If b.Type Is TypeSymbol.Int Then
            Return CInt(left) + CInt(right)
          Else
            Return CStr(left) & CStr(right)
          End If
        Case BoundBinaryOperatorKind.Subtraction : Return CInt(left) - CInt(right)
        Case BoundBinaryOperatorKind.Multiplication : Return CInt(left) * CInt(right)
        Case BoundBinaryOperatorKind.Division : Return CInt(left) \ CInt(right)
        Case BoundBinaryOperatorKind.BitwiseAnd
          If b.Type Is TypeSymbol.Int Then
            Return CInt(left) And CInt(right)
          Else
            Return CBool(left) And CBool(right)
          End If
        Case BoundBinaryOperatorKind.BitwiseOr
          If b.Type Is TypeSymbol.Int Then
            Return CInt(left) Or CInt(right)
          Else
            Return CBool(left) Or CBool(right)
          End If
        Case BoundBinaryOperatorKind.BitwiseXor
          If b.Type Is TypeSymbol.Int Then
            Return CInt(left) Xor CInt(right)
          Else
            Return CBool(left) Xor CBool(right)
          End If
        Case BoundBinaryOperatorKind.LogicalAnd : Return CBool(left) And CBool(right)
        Case BoundBinaryOperatorKind.LogicalOr : Return CBool(left) Or CBool(right)
        Case BoundBinaryOperatorKind.Equals : Return Equals(left, right)
        Case BoundBinaryOperatorKind.NotEquals : Return Not Equals(left, right)
        Case BoundBinaryOperatorKind.Less : Return CInt(left) < CInt(right)
        Case BoundBinaryOperatorKind.Greater : Return CInt(left) > CInt(right)
        Case BoundBinaryOperatorKind.LessOrEquals : Return CInt(left) <= CInt(right)
        Case BoundBinaryOperatorKind.GreaterOrEquals : Return CInt(left) >= CInt(right)
        Case Else
          Throw New Exception($"Unexpected binary operator {b.Op}")
      End Select
    End Function

  End Class

End Namespace