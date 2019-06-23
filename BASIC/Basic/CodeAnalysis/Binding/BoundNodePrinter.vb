Option Infer On

Imports System.CodeDom.Compiler
Imports System.IO
Imports System.Runtime.CompilerServices
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.IO

Namespace Global.Basic.CodeAnalysis.Binding

  Friend Module BoundNodePrinter

    <Extension>
    Public Sub WriteTo(node As BoundNode, writer As TextWriter)
      If TypeOf writer Is IndentedTextWriter Then
        WriteTo(node, DirectCast(writer, IndentedTextWriter))
      Else
        WriteTo(node, New IndentedTextWriter(writer))
      End If
    End Sub

    <Extension>
    Public Sub WriteTo(node As BoundNode, writer As IndentedTextWriter)
      Select Case node.Kind
        Case BoundNodeKind.BlockStatement
          WriteBlockStatement(CType(node, BoundBlockStatement), writer)
        Case BoundNodeKind.VariableDeclaration
          WriteVariableDeclaration(CType(node, BoundVariableDeclaration), writer)
        Case BoundNodeKind.IfStatement
          WriteIfStatement(CType(node, BoundIfStatement), writer)
        Case BoundNodeKind.WhileStatement
          WriteWhileStatement(CType(node, BoundWhileStatement), writer)
        Case BoundNodeKind.DoWhileStatement
          WriteDoWhileStatement(CType(node, BoundDoWhileStatement), writer)
        Case BoundNodeKind.ForStatement
          WriteForStatement(CType(node, BoundForStatement), writer)
        Case BoundNodeKind.LabelStatement
          WriteLabelStatement(CType(node, BoundLabelStatement), writer)
        Case BoundNodeKind.GotoStatement
          WriteGotoStatement(CType(node, BoundGotoStatement), writer)
        Case BoundNodeKind.ConditionalGotoStatement
          WriteConditionalGotoStatement(CType(node, BoundConditionalGotoStatement), writer)
        Case BoundNodeKind.ExpressionStatement
          WriteExpressionStatement(CType(node, BoundExpressionStatement), writer)
        Case BoundNodeKind.ErrorExpression
          WriteErrorExpression(CType(node, BoundErrorExpression), writer)
        Case BoundNodeKind.LiteralExpression
          WriteLiteralExpression(CType(node, BoundLiteralExpression), writer)
        Case BoundNodeKind.VariableExpression
          WriteVariableExpression(CType(node, BoundVariableExpression), writer)
        Case BoundNodeKind.AssignmentExpression
          WriteAssignmentExpression(CType(node, BoundAssignmentExpression), writer)
        Case BoundNodeKind.UnaryExpression
          WriteUnaryExpression(CType(node, BoundUnaryExpression), writer)
        Case BoundNodeKind.BinaryExpression
          WriteBinaryExpression(CType(node, BoundBinaryExpression), writer)
        Case BoundNodeKind.CallExpression
          WriteCallExpression(CType(node, BoundCallExpression), writer)
        Case BoundNodeKind.ConversionExpression
          WriteConversionExpression(CType(node, BoundConversionExpression), writer)
        Case Else
          Throw New Exception($"Unexpected node {node.Kind}")
      End Select
    End Sub

    <Extension>
    Private Sub WriteNestedStatement(writer As IndentedTextWriter, node As BoundStatement)
      Dim needsIndentation = Not (TypeOf node Is BoundBlockStatement)

      If needsIndentation Then
        writer.Indent += 1
      End If

      node.WriteTo(writer)

      If needsIndentation Then
        writer.Indent -= 1
      End If
    End Sub

    <Extension>
    Private Sub WriteNestedExpression(writer As IndentedTextWriter, parentPrecedence As Integer, expression As BoundExpression)
      If TypeOf expression Is BoundUnaryExpression Then
        Dim unary = DirectCast(expression, BoundUnaryExpression)
        writer.WriteNestedExpression(parentPrecedence, SyntaxFacts.GetUnaryOperatorPrecedence(unary.Op.SyntaxKind), unary)
      ElseIf TypeOf expression Is BoundBinaryExpression Then
        Dim binary = DirectCast(expression, BoundBinaryExpression)
        writer.WriteNestedExpression(parentPrecedence, SyntaxFacts.GetBinaryOperatorPrecedence(binary.Op.SyntaxKind), binary)
      Else
        expression.WriteTo(writer)
      End If
    End Sub

    <Extension>
    Private Sub WriteNestedExpression(writer As IndentedTextWriter, parentPrecedence As Integer, currentPrecedence As Integer, expression As BoundExpression)

      Dim needsParenthesis = parentPrecedence >= currentPrecedence

      If needsParenthesis Then
        writer.WritePunctuation("(")
      End If

      expression.WriteTo(writer)

      If needsParenthesis Then
        writer.WritePunctuation(")")
      End If

    End Sub

    Private Sub WriteBlockStatement(node As BoundBlockStatement, writer As IndentedTextWriter)

      writer.WritePunctuation("{")
      writer.WriteLine()
      writer.Indent += 1

      For Each s In node.Statements
        s.WriteTo(writer)
      Next

      writer.Indent -= 1
      writer.WritePunctuation("}")
      writer.WriteLine()

    End Sub

    Private Sub WriteVariableDeclaration(node As BoundVariableDeclaration, writer As IndentedTextWriter)
      writer.WriteKeyword(If(node.Variable.IsReadOnly, "let ", "var "))
      writer.WriteIdentifier(node.Variable.Name)
      writer.WritePunctuation(" = ")
      node.Initializer.WriteTo(writer)
      writer.WriteLine()
    End Sub

    Private Sub WriteIfStatement(node As BoundIfStatement, writer As IndentedTextWriter)
      writer.WriteKeyword("if ")
      node.Condition.WriteTo(writer)
      writer.WriteLine()
      writer.WriteNestedStatement(node.ThenStatement)
      If node.ElseStatement IsNot Nothing Then
        writer.WriteKeyword("else")
        writer.WriteLine()
        writer.WriteNestedStatement(node.ElseStatement)
      End If
    End Sub

    Private Sub WriteWhileStatement(node As BoundWhileStatement, writer As IndentedTextWriter)
      writer.WriteKeyword("while ")
      node.Condition.WriteTo(writer)
      writer.WriteLine()
      writer.WriteNestedStatement(node.Body)
    End Sub

    Private Sub WriteDoWhileStatement(node As BoundDoWhileStatement, writer As IndentedTextWriter)
      writer.WriteKeyword("do")
      writer.WriteLine()
      writer.WriteNestedStatement(node.Body)
      writer.WriteKeyword("while ")
      node.Condition.WriteTo(writer)
      writer.WriteLine()
    End Sub

    Private Sub WriteForStatement(node As BoundForStatement, writer As IndentedTextWriter)
      writer.WriteKeyword("for ")
      writer.WriteIdentifier(node.Variable.Name)
      writer.WritePunctuation(" = ")
      node.LowerBound.WriteTo(writer)
      writer.WriteKeyword(" to ")
      node.UpperBound.WriteTo(writer)
      writer.WriteLine()
      writer.WriteNestedStatement(node.Body)
    End Sub

    Private Sub WriteLabelStatement(node As BoundLabelStatement, writer As IndentedTextWriter)

      Dim unindent = writer.Indent > 0

      If unindent Then
        writer.Indent -= 1
      End If

      writer.WritePunctuation(node.Label.Name)
      writer.WritePunctuation(":")
      writer.WriteLine()

      If unindent Then
        writer.Indent += 1
      End If

    End Sub

    Private Sub WriteGotoStatement(node As BoundGotoStatement, writer As IndentedTextWriter)
      writer.WriteKeyword("goto ")
      writer.WriteIdentifier(node.Label.Name)
      writer.WriteLine()
    End Sub

    Private Sub WriteConditionalGotoStatement(node As BoundConditionalGotoStatement, writer As IndentedTextWriter)
      writer.WriteKeyword("goto ")
      writer.WriteIdentifier(node.Label.Name)
      writer.WriteKeyword(If(node.JumpIfTrue, " if ", " unless "))
      node.Condition.WriteTo(writer)
      writer.WriteLine()
    End Sub

    Private Sub WriteExpressionStatement(node As BoundExpressionStatement, writer As IndentedTextWriter)
      node.Expression.WriteTo(writer)
      writer.WriteLine()
    End Sub

    Private Sub WriteErrorExpression(node As BoundErrorExpression, writer As IndentedTextWriter)
      writer.WriteKeyword("?")
    End Sub

    Private Sub WriteLiteralExpression(node As BoundLiteralExpression, writer As IndentedTextWriter)
      Dim value = node.Value.ToString()
      If node.Type Is TypeSymbol.Bool Then
        writer.WriteKeyword(value)
      ElseIf node.Type Is TypeSymbol.Int Then
        writer.WriteNumber(value)
      ElseIf node.Type Is TypeSymbol.String Then
        value = """" & value.Replace("""", """""") & """"
        writer.WriteString(value)
      Else
        Throw New Exception($"Unexpected type {node.Type}")
      End If
    End Sub

    Private Sub WriteVariableExpression(node As BoundVariableExpression, writer As IndentedTextWriter)
      writer.WriteIdentifier(node.Variable.Name)
    End Sub

    Private Sub WriteAssignmentExpression(node As BoundAssignmentExpression, writer As IndentedTextWriter)
      writer.WriteIdentifier(node.Variable.Name)
      writer.WritePunctuation(" = ")
      node.Expression.WriteTo(writer)
    End Sub

    Private Sub WriteUnaryExpression(node As BoundUnaryExpression, writer As IndentedTextWriter)
      Dim op = SyntaxFacts.GetText(node.Op.SyntaxKind)
      Dim precedence = SyntaxFacts.GetUnaryOperatorPrecedence(node.Op.SyntaxKind)
      writer.WritePunctuation(op)
      writer.WriteNestedExpression(precedence, node.Operand)
    End Sub

    Private Sub WriteBinaryExpression(node As BoundBinaryExpression, writer As IndentedTextWriter)
      Dim op = SyntaxFacts.GetText(node.Op.SyntaxKind)
      Dim precedence = SyntaxFacts.GetBinaryOperatorPrecedence(node.Op.SyntaxKind)
      writer.WriteNestedExpression(precedence, node.Left)
      writer.Write(" ")
      writer.WritePunctuation(op)
      writer.Write(" ")
      writer.WriteNestedExpression(precedence, node.Right)
    End Sub

    Private Sub WriteCallExpression(node As BoundCallExpression, writer As IndentedTextWriter)

      writer.WriteIdentifier(node.Function.Name)
      writer.WritePunctuation("(")

      Dim isFirst = True

      For Each argument In node.Arguments

        If isFirst Then
          isFirst = False
        Else
          writer.WritePunctuation(", ")
        End If

        argument.WriteTo(writer)

      Next

      writer.WritePunctuation(")")

    End Sub

    Private Sub WriteConversionExpression(node As BoundConversionExpression, writer As IndentedTextWriter)
      writer.WriteIdentifier(node.Type.Name)
      writer.WritePunctuation("(")
      node.Expression.WriteTo(writer)
      writer.WritePunctuation(")")
    End Sub

  End Module

End Namespace