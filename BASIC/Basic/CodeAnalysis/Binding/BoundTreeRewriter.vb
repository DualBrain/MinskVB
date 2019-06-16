Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable

Namespace Global.Basic.CodeAnalysis.Binding

  Friend MustInherit Class BoundTreeRewriter

    'Public Overridable Function RewriteNode(node As BoundNode) As BoundNode

    '  If TypeOf node Is BoundStatement Then
    '    Return RewriteStatement(DirectCast(node, BoundStatement))
    '  ElseIf TypeOf node Is BoundExpression Then
    '    Return RewriteExpression(DirectCast(node, BoundExpression))
    '  Else
    '    Throw New Exception($"Unexpected node: {node.Kind}")
    '  End If

    'End Function

    Public Overridable Function RewriteStatement(node As BoundStatement) As BoundStatement
      Select Case node.Kind
        Case BoundNodeKind.BlockStatement : Return Me.RewriteBlockStatement(DirectCast(node, BoundBlockStatement))
        Case BoundNodeKind.VariableDeclaration : Return Me.RewriteVariableDeclaration(DirectCast(node, BoundVariableDeclaration))
        Case BoundNodeKind.IfStatement : Return Me.RewriteIfStatement(DirectCast(node, BoundIfStatement))
        Case BoundNodeKind.WhileStatement : Return Me.RewriteWhileStatement(DirectCast(node, BoundWhileStatement))
        Case BoundNodeKind.ForStatement : Return Me.RewriteForStatement(DirectCast(node, BoundForStatement))
        Case BoundNodeKind.ExpressionStatement : Return Me.RewriteExpressionStatement(DirectCast(node, BoundExpressionStatement))
        Case Else
          Throw New Exception($"Unexpected node: {node.Kind}")
      End Select
    End Function

    Private Function RewriteBlockStatement(node As BoundBlockStatement) As BoundStatement
      Dim builder As ImmutableArray(Of BoundStatement).Builder = Nothing
      For i = 0 To node.Statements.Length - 1
        Dim oldStatement = node.Statements(i)
        Dim newStatement = Me.RewriteStatement(oldStatement)
        If newStatement IsNot oldStatement Then
          If builder Is Nothing Then
            builder = ImmutableArray.CreateBuilder(Of BoundStatement)(node.Statements.Length)
            For j = 0 To i - 1
              builder.Add(node.Statements(j))
            Next
          End If
        End If
        If builder IsNot Nothing Then
          builder.Add(newStatement)
        End If
      Next
      If builder Is Nothing Then
        Return node
      End If
      Return New BoundBlockStatement(builder.MoveToImmutable)
    End Function

    Private Function RewriteVariableDeclaration(node As BoundVariableDeclaration) As BoundStatement
      Dim initializer = Me.RewriteExpression(node.Initializer)
      If initializer Is node.Initializer Then
        Return node
      End If
      Return New BoundVariableDeclaration(node.Variable, initializer)
    End Function

    Private Function RewriteIfStatement(node As BoundIfStatement) As BoundStatement
      Dim condition = Me.RewriteExpression(node.Condition)
      Dim thenStatement = Me.RewriteStatement(node.ThenStatement)
      Dim elseStatement = If(node.ElseStatement Is Nothing, Nothing, Me.RewriteStatement(node.ElseStatement))
      If condition Is node.Condition AndAlso
               thenStatement Is node.ThenStatement AndAlso
               elseStatement Is node.ElseStatement Then
        Return node
      End If
      Return New BoundIfStatement(condition, thenStatement, elseStatement)
    End Function

    Private Function RewriteWhileStatement(node As BoundWhileStatement) As BoundStatement
      Dim condition = Me.RewriteExpression(node.Condition)
      Dim body = Me.RewriteStatement(node.Body)
      If condition Is node.Condition AndAlso body Is node.Body Then
        Return node
      End If
      Return New BoundWhileStatement(condition, body)
    End Function

    Private Function RewriteForStatement(node As BoundForStatement) As BoundStatement
      Dim lowerBound = Me.RewriteExpression(node.LowerBound)
      Dim upperBound = Me.RewriteExpression(node.UpperBound)
      Dim body = Me.RewriteStatement(node.Body)
      If lowerBound Is node.LowerBound AndAlso
               upperBound Is node.UpperBound AndAlso
               body Is node.Body Then
        Return node
      End If
      Return New BoundForStatement(node.Variable, lowerBound, upperBound, body)
    End Function

    Private Function RewriteExpressionStatement(node As BoundExpressionStatement) As BoundStatement
      Dim expression = Me.RewriteExpression(node.Expression)
      If expression Is node.Expression Then
        Return node
      Else
        Return New BoundExpressionStatement(expression)
      End If
    End Function

    Public Overridable Function RewriteExpression(node As BoundExpression) As BoundExpression
      Select Case node.Kind
        Case BoundNodeKind.LiteralExpression : Return Me.RewriteLiteralExpression(DirectCast(node, BoundLiteralExpression))
        Case BoundNodeKind.VariableExpression : Return Me.RewriteVariableExpression(DirectCast(node, BoundVariableExpression))
        Case BoundNodeKind.AssignmentExpression : Return Me.RewriteAssignmentExpression(DirectCast(node, BoundAssignmentExpression))
        Case BoundNodeKind.UnaryExpression : Return Me.RewriteUnaryExpression(DirectCast(node, BoundUnaryExpression))
        Case BoundNodeKind.BinaryExpression : Return Me.RewriteBinaryExpression(DirectCast(node, BoundBinaryExpression))
        Case Else
          Throw New Exception($"Unexpected node: {node.Kind}")
      End Select
    End Function

    Protected Overridable Function RewriteLiteralExpression(node As BoundLiteralExpression) As BoundExpression
      Return node
    End Function

    Protected Overridable Function RewriteVariableExpression(node As BoundVariableExpression) As BoundExpression
      Return node
    End Function

    Protected Overridable Function RewriteAssignmentExpression(node As BoundAssignmentExpression) As BoundExpression
      Dim expression = Me.RewriteExpression(node.Expression)
      If expression Is node.Expression Then
        Return node
      Else
        Return New BoundAssignmentExpression(node.Variable, expression)
      End If
    End Function

    Protected Overridable Function RewriteUnaryExpression(node As BoundUnaryExpression) As BoundExpression
      Dim operand = Me.RewriteExpression(node.Operand)
      If operand Is node.Operand Then
        Return node
      Else
        Return New BoundUnaryExpression(node.Op, operand)
      End If
    End Function

    Protected Overridable Function RewriteBinaryExpression(node As BoundBinaryExpression) As BoundExpression
      Dim left = Me.RewriteExpression(node.Left)
      Dim right = Me.RewriteExpression(node.Right)
      If left Is node.Left AndAlso
               right Is node.Right Then
        Return node
      Else
        Return New BoundBinaryExpression(left, node.Op, right)
      End If
    End Function

  End Class

End Namespace