Option Explicit On
Option Strict On
Option Infer On

Imports Xunit
Imports Basic.CodeAnalysis
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.Tests.CodeAnalysis

  Public Class EvaluationTests

    <Theory>
    <InlineData("1", 1)>
    <InlineData("+1", 1)>
    <InlineData("-1", -1)>
    <InlineData("~1", -2)>
    <InlineData("14 + 12", 26)>
    <InlineData("12 - 3", 9)>
    <InlineData("4 * 2", 8)>
    <InlineData("9 / 3", 3)>
    <InlineData("(10)", 10)>
    <InlineData("12 == 3", False)>
    <InlineData("3 == 3", True)>
    <InlineData("12 != 3", True)>
    <InlineData("3 != 3", False)>
    <InlineData("12 <> 3", True)>
    <InlineData("3 <> 3", False)>
    <InlineData("3 < 4", True)>
    <InlineData("5 < 4", False)>
    <InlineData("4 <= 4", True)>
    <InlineData("4 <= 5", True)>
    <InlineData("5 <= 4", False)>
    <InlineData("4 > 3", True)>
    <InlineData("4 > 5", False)>
    <InlineData("4 >= 4", True)>
    <InlineData("5 >= 4", True)>
    <InlineData("4 >= 5", False)>
    <InlineData("1 | 2", 3)>
    <InlineData("1 | 0", 1)>
    <InlineData("1 & 3", 1)>
    <InlineData("1 & 0", 0)>
    <InlineData("1 ^ 0", 1)>
    <InlineData("0 ^ 1", 1)>
    <InlineData("1 ^ 3", 2)>
    <InlineData("false == false", True)>
    <InlineData("true == false", False)>
    <InlineData("false != false", False)>
    <InlineData("true != false", True)>
    <InlineData("true && true", True)>
    <InlineData("false or false", False)>
    <InlineData("false | false", False)>
    <InlineData("false | true", True)>
    <InlineData("true | false", True)>
    <InlineData("true | true", True)>
    <InlineData("false & false", False)>
    <InlineData("false & true", False)>
    <InlineData("true & false", False)>
    <InlineData("true & true", True)>
    <InlineData("false ^ false", False)>
    <InlineData("true ^ false", True)>
    <InlineData("false ^ true", True)>
    <InlineData("true ^ true", False)>
    <InlineData("true", True)>
    <InlineData("false", False)>
    <InlineData("!true", False)>
    <InlineData("not true", False)>
    <InlineData("!false", True)>
    <InlineData("var a = 10 return a", 10)>
    <InlineData(ChrW(34) + "test" + ChrW(34), "test")>
    <InlineData(ChrW(34) + "te" + ChrW(34) + ChrW(34) + "st" & ChrW(34), "te" & ChrW(34) & "st")>
    <InlineData(ChrW(34) + "test" & ChrW(34) & " == " & ChrW(34) & "test" & ChrW(34), True)>
    <InlineData(ChrW(34) + "test" & ChrW(34) & " != " & ChrW(34) & "test" & ChrW(34), False)>
    <InlineData(ChrW(34) + "test" & ChrW(34) & " == " & ChrW(34) & "abc" & ChrW(34), False)>
    <InlineData(ChrW(34) + "test" & ChrW(34) & " != " & ChrW(34) & "abc" & ChrW(34), True)>
    <InlineData(ChrW(34) + "test" & ChrW(34) & " + " & ChrW(34) & "abc" & ChrW(34), "testabc")>
    <InlineData("not false", True)>
    <InlineData("false || true", True)>
    <InlineData("false or true", True)>
    <InlineData("true || true", True)>
    <InlineData("true or true", True)>
    <InlineData("false || false", False)>
    <InlineData("false or false", False)>
    <InlineData("true && false", False)>
    <InlineData("true and false", False)>
    <InlineData("false && false", False)>
    <InlineData("false and false", False)>
    <InlineData("true && true", True)>
    <InlineData("true and true", True)>
    <InlineData("{ var a = 10 return a * a }", 100)>
    <InlineData("{ var a = 0 return (a = 10) * a }", 100)>
    <InlineData("{ var a = 0 if a == 0 a = 10 return a}", 10)>
    <InlineData("{ var a = 0 if a == 4 a = 10 return a}", 0)>
    <InlineData("{ var a = 0 if a == 0 a = 10 else a=5 return a}", 10)>
    <InlineData("{ var a = 0 if a == 4 a = 10 else a=5 return a}", 5)>
    <InlineData("{ var i = 10 var result = 0 while i > 0 { result = result + i i = i - 1} return result }", 55)>
    <InlineData("{ var result = 0 for i = 1 to 10 { result = result + i} return result }", 55)>
    <InlineData("{ var a = 10 for i = 1 to (a = a - 1) { } return a }", 9)>
    <InlineData("{ var a = 0 do a = a + 1 while a < 10 return a}", 10)>
    <InlineData("{ var i = 0 while i < 5 { i = i + 1 if i == 5 continue } return i }", 5)>
    <InlineData("{ var i = 0 do { i = i + 1 if i == 5 continue } while i < 5 return i }", 5)>
    Public Sub SyntaxFact_GetText_RoundTrips(text As String, expectedValue As Object)
      AssertValue(text, expectedValue)
    End Sub

    <Fact>
    Public Sub Evaluator_VariableDeclaration_Reports_Redeclaration()

      Dim text = "
        {
          var x = 10
          var y = 100
          {
            var x = 10
          }
          var [x] = 5
        }"

      Dim diagnostics = "
        'x' is already declared."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_BlockStatement_NoInfiniteLoop()

      Dim text = "
        {
         [)]
[]
"

      Dim diagnostics = "
        Unexpected token <CloseParenToken>, expected <IdentifierToken>.
        Unexpected token <EndOfFileToken>, expected <CloseBraceToken>."

      AssertDiagnostics(text, diagnostics)

    End Sub


    <Fact>
    Public Sub Evaluator_InvokeFunctionArguments_Missing()

      Dim text = "
        print([)]"

      Dim diagnostics = "
        Function 'print' requires 1 arguments but was given 0."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_InvokeFunctionArguments_Exceeding()

      Dim text = $"
        print({ChrW(34)}Hello{ChrW(34)}[, {ChrW(34)} {ChrW(34)}, {ChrW(34)} world!{ChrW(34)}])"

      Dim diagnostics = "
        Function 'print' requires 1 arguments but was given 3."

      AssertDiagnostics(text, diagnostics)

    End Sub



    <Fact>
    Public Sub Evaluator_InvokeFunctionArguments_NoInfiniteLoop()

      Dim text = "
                print(""Hi""[[=]][)]
            "

      Dim diagnostics = "
                Unexpected token <EqualsToken>, expected <CloseParenToken>.
                Unexpected token <EqualsToken>, expected <IdentifierToken>.
                Unexpected token <CloseParenToken>, expected <IdentifierToken>.
            "

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_FunctionParameters_NoInfiniteLoop()

      Dim text = "
                function hi(name: string[[[=]]][)]
                {
                    print(""Hi "" + name + ""!"" )
                }[]
            "

      Dim diagnostics = "
                Unexpected token <EqualsToken>, expected <CloseParenToken>.
                Unexpected token <EqualsToken>, expected <OpenBraceToken>.
                Unexpected token <EqualsToken>, expected <IdentifierToken>.
                Unexpected token <CloseParenToken>, expected <IdentifierToken>.
                Unexpected token <EndOfFileToken>, expected <CloseBraceToken>.
            "

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_FunctionReturn_Missing()

      Dim text = "
        function [add](a: int, b: int): int
        {
        }"

      Dim diagnostics = "
        Not all code paths return a value."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_IfStatement_Reports_CannotConvert()

      Dim text = "
        {
          var x = 0
          if [10]
            x = 10
        }"

      Dim diagnostics = "
        Cannot convert type 'int' to 'bool'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_WhileStatement_Reports_CannotConvert()

      Dim text = "
        {
          var x = 0
          while [10]
            x = 10
        }"

      Dim diagnostics = "
        Cannot convert type 'int' to 'bool'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Void_Function_Should_Not_Return_Value()

      Dim text = "
        function test()
        {
          return [1]
        }"

      Dim diagnostics = "
        Since the function 'test' does not return a value, the 'return' keyword cannot be followed by an expression."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Function_With_ReturnValue_Should_Not_Return_Void()

      Dim text = "
        function test(): int
        {
          [return]
        }"

      Dim diagnostics = "
        An expression of type 'int' is expected."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Not_All_Code_Paths_Return_Value()

      Dim text = "
        function [test](n: int): bool
        {
          if (n > 10)
            return true
        }"

      Dim diagnostics = "
        Not all code paths return a value."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Expression_Must_Have_Value()

      Dim text = "
        function test(n: int)
        {
          return
        }
        let value = [test(100)]"

      Dim diagnostics = "
        Expression must have a value."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Theory, InlineData("[break]", "break"), InlineData("[continue]", "continue")>
    Public Sub Evaluator_Invalid_Break_Or_Continue(ByVal text As String, ByVal keyword As String)

      Dim diagnostics = $"
        The keyword '{keyword}' can only be used inside of loops."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Script_Return()

      Dim text = "
        return"

      AssertValue(text, "")

    End Sub

    <Fact>
    Public Sub Evaluator_Parameter_Already_Declared()

      Dim text = "
        function sum(a: int, b: int, [a: int]): int
        {
          return a + b + c
        }"

      Dim diagnostics = "
        A parameter with the name 'a' already exists."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Function_Must_Have_Name()

      Dim text = "
        function [(]a: int, b: int): int
        {
          return a + b
        }"

      Dim diagnostics = "
        Unexpected token <OpenParenToken>, expected <IdentifierToken>."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Wrong_Argument_Type()

      Dim text = "
        function test(n: int): bool
        {
          return n > 10
        }
        let t = ""string""
        test([t])"

      Dim diagnostics = "
        Cannot convert type 'string' to 'int'. An explicit conversion exists (are you missing a cast?)"

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Bad_Type()

      Dim text = "
        function test(n: [invalidtype])
        {
        }"

      Dim diagnostics = "
        Type 'invalidtype' doesn't exist."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_DoWhileStatement_Reports_CannotConvert()

      Dim text = "
        {
          var x = 0
          do
            x = 10
          while [10]
        }"

      Dim diagnostics = "
        Cannot convert type 'int' to 'bool'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_ForStatement_Reports_CannotConvert_LowerBound()

      Dim text = "
        {
          var result = 0
          for i = [false] to 10
            result = result + i
        }"

      Dim diagnostics = "
        Cannot convert type 'bool' to 'int'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_ForStatement_Reports_CannotConvert_UpperBound()

      Dim text = "
        {
          var result = 0
          for i = 1 to [true]
            result = result + i
        }"

      Dim diagnostics = "
        Cannot convert type 'bool' to 'int'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_NameExpression_Reports_Undefined()

      Dim text = "[x] * 10"

      Dim diagnostics = "Variable 'x' doesn't exist."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_NameExpression_Reports_NoErrorForInsertedToken()

      Dim text = "1 + []"

      Dim diagnostics = "Unexpected token <EndOfFileToken>, expected <IdentifierToken>."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_UnaryExpression_Reports_Undefined()

      Dim text = "[+]true"

      Dim diagnostics = "Unary operator '+' is not defined for type 'bool'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_BinaryExpression_Reports_Undefined()

      Dim text = "10 [+] false"

      Dim diagnostics = "Binary operator '+' is not defined for type 'int' and 'bool'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_AssignmentExpression_Reports_NotAVariable()

      Dim text = "[print] = 42"

      Dim diagnostics = "
        'print' is not a variable."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_AssignmentExpression_Reports_Undefined()

      Dim text = "[x] = 10"

      Dim diagnostics = "Variable 'x' doesn't exist."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_AssignmentExpression_Reports_CannotAssign()

      Dim text = "
        {
          let x = 10
          x [=] 0
        }"

      Dim diagnostics = "Variable 'x' is read-only and cannot be assigned to."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_AssignmentExpression_Reports_CannotConvert()

      Dim text = "
        {
          let x = 10
          x [=] [true]
        }"

      Dim diagnostics = "
        Variable 'x' is read-only and cannot be assigned to.
        Cannot convert type 'bool' to 'int'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_CallExpression_Reports_Undefined()

      Dim text = "[foo](42)"

      Dim diagnostics = "
        Function 'foo' doesn't exist."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_CallExpression_Reports_NotAFunction()

      Dim text = "
        {
          let foo = 42
          [foo](42)
        }"

      Dim diagnostics = "
        'foo' is not a function."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Variables_Can_Shadow_Functions()

      Dim text = "
        {
          let print = 42
          [print](""test"")
        }"

      Dim diagnostics = "
        'print' is not a function."

      AssertDiagnostics(text, diagnostics)

    End Sub

    Private Shared Sub AssertValue(text As String, expectedValue As Object)

      Dim tree = SyntaxTree.Parse(text)
      Dim compilation = Basic.CodeAnalysis.Compilation.CreateScript(Nothing, tree)
      Dim variables = New Dictionary(Of VariableSymbol, Object)
      Dim result = compilation.Evaluate(variables)

      Assert.Empty(result.Diagnostics)

      Assert.Equal(expectedValue, result.Value)

    End Sub

    Private Sub AssertDiagnostics(text As String, diagnosticText As String)

      Dim at = AnnotatedText.Parse(text)
      Dim tree = SyntaxTree.Parse(at.Text)
      Dim compilation = Basic.CodeAnalysis.Compilation.CreateScript(Nothing, tree)
      Dim result = compilation.Evaluate(New Dictionary(Of VariableSymbol, Object))

      Dim expectedDiagnostics = AnnotatedText.UnindentLines(diagnosticText)

      If at.Spans.Length <> expectedDiagnostics.Length Then
        Throw New Exception("ERROR: Must mark as many spans as there are expected diagnostics.")
      End If

      Assert.Equal(expectedDiagnostics.Length, result.Diagnostics.Length)

      For i = 0 To expectedDiagnostics.Length - 1

        Dim expectedMessage = expectedDiagnostics(i)
        Dim actualMessage = result.Diagnostics(i).Message
        Assert.Equal(expectedMessage, actualMessage)

        Dim expectedSpan = at.Spans(i)
        Dim actualSpan = result.Diagnostics(i).Location.Span
        Assert.Equal(expectedSpan, actualSpan)

      Next

    End Sub

  End Class

End Namespace