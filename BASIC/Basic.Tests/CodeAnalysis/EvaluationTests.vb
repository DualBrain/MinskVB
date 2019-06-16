Option Explicit On
Option Strict On
Option Infer On

Imports Xunit
Imports Basic.CodeAnalysis
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.Tests.CodeAnalysis

  Public Class EvaluationTests

    <Theory>
    <InlineData("1", 1)>
    <InlineData("+1", 1)>
    <InlineData("-1", -1)>
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
 _
    <InlineData("3 < 4", True)>
    <InlineData("5 < 4", False)>
    <InlineData("4 <= 4", True)>
    <InlineData("4 <= 5", True)>
    <InlineData("5 <= 4", False)>
 _
    <InlineData("4 > 3", True)>
    <InlineData("4 > 5", False)>
    <InlineData("4 >= 4", True)>
    <InlineData("5 >= 4", True)>
    <InlineData("4 >= 5", False)>
 _
    <InlineData("false == false", True)>
    <InlineData("true == false", False)>
    <InlineData("false != false", False)>
    <InlineData("true != false", True)>
    <InlineData("true", True)>
    <InlineData("false", False)>
    <InlineData("!true", False)>
    <InlineData("not true", False)>
    <InlineData("!false", True)>
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
    <InlineData("{ var a = 0 (a = 10) * a }", 100)>
    <InlineData("{ var a = 0 if a == 0 a = 10 a}", 10)>
    <InlineData("{ var a = 0 if a == 4 a = 10 a}", 0)>
    <InlineData("{ var a = 0 if a == 0 a = 10 else a=5 a}", 10)>
    <InlineData("{ var a = 0 if a == 4 a = 10 else a=5 a}", 5)>
    <InlineData("{ var i = 10 var result = 0 while i > 0 { result = result + i i = i - 1} result }", 55)>
    <InlineData("{ var result = 0 for i = 1 to 10 { result = result + i} result }", 55)>
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
        Variable 'x' is already declared."

      Me.AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Name_Reports_Undefined()

      Dim text = "[x] * 10"

      Dim diagnostics = "Variable 'x' doesn't exist."

      Me.AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Assigned_Reports_Undefined()

      Dim text = "[x] = 10"

      Dim diagnostics = "Variable 'x' doesn't exist."

      Me.AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Assigned_Reports_CannotAssign()

      Dim text = "
        {
          let x = 10
          x [=] 0
        }"

      Dim diagnostics = "Variable 'x' is read-only and cannot be assigned to."

      Me.AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Assigned_Reports_CannotConvert()

      Dim text = "
        {
          let x = 10
          x [=] [true]
        }"

      Dim diagnostics = "
        Variable 'x' is read-only and cannot be assigned to.
        Cannot convert type 'System.Boolean' to 'System.Int32'."

      Me.AssertDiagnostics(text, diagnostics)

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
        Cannot convert type 'System.Int32' to 'System.Boolean'."

      Me.AssertDiagnostics(text, diagnostics)

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
        Cannot convert type 'System.Int32' to 'System.Boolean'."

      Me.AssertDiagnostics(text, diagnostics)

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
        Cannot convert type 'System.Boolean' to 'System.Int32'."

      Me.AssertDiagnostics(text, diagnostics)

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
        Cannot convert type 'System.Boolean' to 'System.Int32'."

      Me.AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Unary_Reports_Undefined()

      Dim text = "[+]true"

      Dim diagnostics = "Unary operator '+' is not defined for type 'System.Boolean'."

      Me.AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Binary_Reports_Undefined()

      Dim text = "10 [+] false"

      Dim diagnostics = "Binary operator '+' is not defined for type 'System.Int32' and 'System.Boolean'."

      Me.AssertDiagnostics(text, diagnostics)

    End Sub

    Private Shared Sub AssertValue(text As String, expectedValue As Object)

      Dim tree = SyntaxTree.Parse(text)
      Dim compilation = New Compilation(tree)
      Dim variables = New Dictionary(Of VariableSymbol, Object)
      Dim result = compilation.Evaluate(variables)

      Assert.Empty(result.Diagnostics)

      Assert.Equal(expectedValue, result.Value)

    End Sub

    Private Sub AssertDiagnostics(text As String, diagnosticText As String)

      Dim at = AnnotatedText.Parse(text)
      Dim tree = SyntaxTree.Parse(at.Text)
      Dim compilation = New Compilation(tree)
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
        Dim actualSpan = result.Diagnostics(i).Span
        Assert.Equal(expectedSpan, actualSpan)

      Next

    End Sub

  End Class

End Namespace