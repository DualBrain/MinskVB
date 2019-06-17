Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Binding
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis.Lowering

  Friend NotInheritable Class Lowerer
    Inherits BoundTreeRewriter

    Private Sub New()
    End Sub

    Public Shared Function Lower(statement As BoundStatement) As BoundStatement
      Dim lowerer = New Lowerer
      Return lowerer.RewriteStatement(statement)
    End Function

    Protected Overrides Function RewriteForStatement(node As BoundForStatement) As BoundStatement

      '
      ' for i = <lower> to <upper>
      '     <boby>
      '
      '  ------>
      '
      ' {
      '   var <var> = <lower>
      '   while (<var> <= <upper>)
      '   {
      '     <body>
      '     <var> = <var> + 1
      '   }
      ' }

      Dim variableDeclaration = New BoundVariableDeclaration(node.Variable, node.LowerBound)
      Dim variableExpression = New BoundVariableExpression(node.Variable)
      Dim condition = New BoundBinaryExpression(
        variableExpression,
        BoundBinaryOperator.Bind(SyntaxKind.LessThanEqualsToken, GetType(Integer), GetType(Integer)),
        node.UpperBound)
      Dim increment = New BoundExpressionStatement(
        New BoundAssignmentExpression(
          node.Variable,
          New BoundBinaryExpression(
            variableExpression,
            BoundBinaryOperator.Bind(SyntaxKind.PlusToken, GetType(Integer), GetType(Integer)),
            New BoundLiteralExpression(1))))
      Dim whileBody = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(node.Body, increment))
      Dim whileStatement = New BoundWhileStatement(condition, whileBody)
      Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(variableDeclaration, whileStatement))
      Return Me.RewriteStatement(result)
    End Function

  End Class

End Namespace
