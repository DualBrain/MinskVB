Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Binding
Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis

  Friend NotInheritable Class Evaluator

    Private ReadOnly m_program As BoundProgram
    Private ReadOnly m_globals As Dictionary(Of VariableSymbol, Object)
    Private ReadOnly m_functions As New Dictionary(Of FunctionSymbol, BoundBlockStatement)
    Private ReadOnly m_locals As New Stack(Of Dictionary(Of VariableSymbol, Object))
    Private m_random As Random

    Private m_lastValue As Object

    Sub New(program As BoundProgram, variables As Dictionary(Of VariableSymbol, Object))
      Me.m_program = program
      m_globals = variables
      m_locals.Push(New Dictionary(Of VariableSymbol, Object))

      Dim currrent = program
      While currrent IsNot Nothing
        For Each kv In currrent.Functions
          Dim func = kv.Key
          Dim body = kv.Value
          m_functions.Add(func, body)
        Next
        currrent = currrent.Previous
      End While

    End Sub

    Public Function Evaluate() As Object
      Dim func = If(m_program.MainFunction, m_program.ScriptFunction)
      If func Is Nothing Then
        Return Nothing
      Else
        Dim body = m_functions(func)
        Return EvaluateStatement(body)
      End If
    End Function

    Private Function EvaluateStatement(body As BoundBlockStatement) As Object

      Dim labelToIndex = New Dictionary(Of BoundLabel, Integer)
      For i = 0 To body.Statements.Length - 1
        If TypeOf body.Statements(i) Is BoundLabelStatement Then
          labelToIndex.Add(DirectCast(body.Statements(i), BoundLabelStatement).Label, i + 1)
        End If
      Next

      Dim index = 0
      While index < body.Statements.Length
        Dim s = body.Statements(index)
        Select Case s.Kind
          Case BoundNodeKind.NopStatement : index += 1
          Case BoundNodeKind.VariableDeclaration : EvaluateVariableDeclaration(DirectCast(s, BoundVariableDeclaration)) : index += 1
          Case BoundNodeKind.ExpressionStatement : EvaluateExpressionStatement(DirectCast(s, BoundExpressionStatement)) : index += 1
          Case BoundNodeKind.GotoStatement
            Dim gs As BoundGotoStatement = DirectCast(s, BoundGotoStatement)
            index = labelToIndex(gs.Label)
          Case BoundNodeKind.ConditionalGotoStatement
            Dim cgs = DirectCast(s, BoundConditionalGotoStatement)
            Dim condition = CBool(EvaluateExpression(cgs.Condition))
            'If (condition AndAlso Not cgs.JumpIfFalse) OrElse
            '   (Not condition AndAlso cgs.JumpIfFalse) Then
            If condition = cgs.JumpIfTrue Then
              index = labelToIndex(cgs.Label)
            Else
              index += 1
            End If
          Case BoundNodeKind.LabelStatement : index += 1
          Case BoundNodeKind.ReturnStatement
            Dim rs = CType(s, BoundReturnStatement)
            m_lastValue = If(rs.Expression Is Nothing, Nothing, EvaluateExpression(rs.Expression))
            Return m_lastValue
          Case Else
            Throw New Exception($"Unexpected statement {s.Kind}")
        End Select
      End While

      Return m_lastValue

    End Function

    Private Sub EvaluateVariableDeclaration(node As BoundVariableDeclaration)
      Dim value = EvaluateExpression(node.Initializer)
      m_lastValue = value
      Assign(node.Variable, value)
    End Sub

    Private Sub EvaluateExpressionStatement(node As BoundExpressionStatement)
      m_lastValue = EvaluateExpression(node.Expression)
    End Sub

    Private Function EvaluateExpression(node As BoundExpression) As Object

      Select Case node.Kind
        Case BoundNodeKind.LiteralExpression : Return EvaluateLiteralExpression(node)
        Case BoundNodeKind.VariableExpression : Return EvaluateVariableExpression(DirectCast(node, BoundVariableExpression))
        Case BoundNodeKind.AssignmentExpression : Return EvaluateAssignmentExpression(DirectCast(node, BoundAssignmentExpression))
        Case BoundNodeKind.UnaryExpression : Return EvaluateUnaryExpression(node)
        Case BoundNodeKind.BinaryExpression : Return EvaluateBinaryExpression(node)
        Case BoundNodeKind.CallExpression : Return EvaluateCallExpression(DirectCast(node, BoundCallExpression))
        Case BoundNodeKind.ConversionExpression : Return EvaluateConversionExpression(DirectCast(node, BoundConversionExpression))
        Case Else
          Throw New Exception($"Unexpected node {node.Kind}")
      End Select

    End Function

    Private Function EvaluateLiteralExpression(node As BoundExpression) As Object
      Return DirectCast(node, BoundLiteralExpression).Value
    End Function

    Private Function EvaluateVariableExpression(v As BoundVariableExpression) As Object
      If v.Variable.Kind = SymbolKind.GlobalVariable Then
        Return m_globals(v.Variable)
      Else
        Dim locals = m_locals.Peek()
        Return locals(v.Variable)
      End If
    End Function

    Private Function EvaluateAssignmentExpression(a As BoundAssignmentExpression) As Object
      Dim value = EvaluateExpression(a.Expression)
      Assign(a.Variable, value)
      Return value
    End Function

    Private Function EvaluateUnaryExpression(node As BoundExpression) As Object
      Dim u = DirectCast(node, BoundUnaryExpression)
      Dim operand = EvaluateExpression(u.Operand)
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
      Dim left = EvaluateExpression(b.Left)
      Dim right = EvaluateExpression(b.Right)
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

    Private Function EvaluateCallExpression(node As BoundCallExpression) As Object

#If BASIC Then

      'Public ReadOnly Asc As New FunctionSymbol("ASC", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.Int)
      'Public ReadOnly Chr As New FunctionSymbol("CHR$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Int)), TypeSymbol.String)
      'Public ReadOnly Instr As New FunctionSymbol("INSTR", ImmutableArray.Create(New ParameterSymbol("start", TypeSymbol.Int), New ParameterSymbol("string1", TypeSymbol.String), New ParameterSymbol("string2", TypeSymbol.String)), TypeSymbol.Int)
      'Public ReadOnly LCase As New FunctionSymbol("LCASE$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String), New ParameterSymbol("num", TypeSymbol.Int)), TypeSymbol.String)
      'Public ReadOnly Len As New FunctionSymbol("LEN", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.Int)
      'Public ReadOnly MidFunction As New FunctionSymbol("MID$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String), New ParameterSymbol("start", TypeSymbol.Int), New ParameterSymbol("num", TypeSymbol.Int)), TypeSymbol.String)
      'Public ReadOnly Right As New FunctionSymbol("RIGHT$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String), New ParameterSymbol("num", TypeSymbol.Int)), TypeSymbol.String)
      'Public ReadOnly Space As New FunctionSymbol("SPACE$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Int)), TypeSymbol.String)
      'Public ReadOnly Str As New FunctionSymbol("STR$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Int)), TypeSymbol.String)
      'Public ReadOnly StringFunctionA As New FunctionSymbol("STRING$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Int), New ParameterSymbol("code", TypeSymbol.Int)), TypeSymbol.String)
      'Public ReadOnly StringFunctionB As New FunctionSymbol("STRING$", ImmutableArray.Create(New ParameterSymbol("string1", TypeSymbol.String), New ParameterSymbol("code", TypeSymbol.Int)), TypeSymbol.String)
      'Public ReadOnly UCase As New FunctionSymbol("UCASE$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.String)
      'Public ReadOnly Val As New FunctionSymbol("VAL", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.Int)


      If node.Function Is BuiltinFunctions.Hex Then
        Return CInt(EvaluateExpression(node.Arguments(0))).ToString("X")
      ElseIf node.Function Is BuiltinFunctions.Oct Then
        Return Convert.ToString(CInt(EvaluateExpression(node.Arguments(0))), 8)
      ElseIf node.Function Is BuiltinFunctions.Asc Then
        Return Strings.AscW(CStr(EvaluateExpression(node.Arguments(0))))
      ElseIf node.Function Is BuiltinFunctions.Chr Then
        Return Strings.ChrW(CInt(EvaluateExpression(node.Arguments(0))))
      ElseIf node.Function Is BuiltinFunctions.Instr Then
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.LCase Then
        Return CStr(EvaluateExpression(node.Arguments(0))).ToLower
      ElseIf node.Function Is BuiltinFunctions.Len Then
        Return CStr(EvaluateExpression(node.Arguments(0))).Length
      ElseIf node.Function Is BuiltinFunctions.MidFunction Then
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Right Then
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Space Then
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Str Then
        Return CStr(CDbl(EvaluateExpression(node.Arguments(0))))
      ElseIf node.Function Is BuiltinFunctions.StringFunction Then
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.UCase Then
        Return CStr(EvaluateExpression(node.Arguments(0))).ToUpper
      ElseIf node.Function Is BuiltinFunctions.Val Then
        Return Convert.ToDouble(CStr(EvaluateExpression(node.Arguments(0))))
      End If

#End If

      If node.Function Is BuiltinFunctions.Input Then
        Return Console.ReadLine()
      ElseIf node.Function Is BuiltinFunctions.Print Then
        Dim value = EvaluateExpression(node.Arguments(0))
        Console.WriteLine(value)
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.Rnd Then
        Dim max = CInt(EvaluateExpression(node.Arguments(0)))
        If m_random Is Nothing Then m_random = New Random
        Return m_random.Next(max)
      Else
        Dim locals = New Dictionary(Of VariableSymbol, Object)
        For i = 0 To node.Arguments.Length - 1
          Dim parameter = node.Function.Parameters(i)
          Dim value = EvaluateExpression(node.Arguments(i))
          locals.Add(parameter, value)
        Next
        m_locals.Push(locals)
        Dim statement = m_functions(node.Function)
        Dim result = EvaluateStatement(statement)
        m_locals.Pop()
        Return result
      End If

    End Function

    Private Function EvaluateConversionExpression(node As BoundConversionExpression) As Object
      Dim value = EvaluateExpression(node.Expression)
      If node.Type Is TypeSymbol.Any Then
        Return value
      ElseIf node.Type Is TypeSymbol.Bool Then
        Return Convert.ToBoolean(value)
      ElseIf node.Type Is TypeSymbol.Int Then
        Return Convert.ToInt32(value)
      ElseIf node.Type Is TypeSymbol.String Then
        Return Convert.ToString(value)
      Else
        Throw New Exception($"Unexpected type {node.Type}.")
      End If
    End Function

    Private Sub Assign(variable As VariableSymbol, value As Object)
      If variable.Kind = SymbolKind.GlobalVariable Then
        m_globals(variable) = value
      Else
        Dim locals = m_locals.Peek()
        locals(variable) = value
      End If
    End Sub

  End Class

End Namespace