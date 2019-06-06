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
    <InlineData("(a = 10) * a", 100)>
    Public Sub SyntaxFact_GetText_RoundTrips(text As String, expectedValue As Object)

      Dim tree = SyntaxTree.Parse(text)
      Dim compilation = New Compilation(tree)
      Dim variables = New Dictionary(Of VariableSymbol, Object)
      Dim result = compilation.Evaluate(variables)

      Assert.Empty(result.Diagnostics)

      Assert.Equal(expectedValue, result.Value)

    End Sub

  End Class

End Namespace