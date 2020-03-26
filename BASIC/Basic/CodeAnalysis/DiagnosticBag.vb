Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.CodeAnalysis

  Friend NotInheritable Class DiagnosticBag
    Implements IEnumerable(Of Diagnostic)

    Private ReadOnly m_diagnostics As New List(Of Diagnostic)

    Public Function GetEnumerator() As IEnumerator(Of Diagnostic) Implements IEnumerable(Of Diagnostic).GetEnumerator
      Return m_diagnostics.GetEnumerator
    End Function

    Private Function IEnumerable_GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
      Return GetEnumerator
    End Function

    Private Sub Report(span As TextSpan, message As String)
      Dim diagnostic = New Diagnostic(span, message)
      m_diagnostics.Add(diagnostic)
    End Sub

    Public Sub AddRange(diagnostics As DiagnosticBag)
      m_diagnostics.AddRange(diagnostics.m_diagnostics)
    End Sub

    Public Sub Concat(diagnostics As DiagnosticBag)
      m_diagnostics.Concat(diagnostics.m_diagnostics)
    End Sub

    Public Sub ReportInvalidNumber(span As TextSpan, text As String, type As TypeSymbol)
      Report(span, $"The number {text} isn't valid {type}.")
    End Sub

    Public Sub ReportBadCharacter(position As Integer, character As Char)
      Report(New TextSpan(position, 1), $"Bad character input: '{character}'.")
    End Sub

    Public Sub ReportUnterminatedString(span As TextSpan)
      Report(span, $"Unterminated string literal.")
    End Sub

    Public Sub ReportUnexpectedToken(span As TextSpan, actualKind As SyntaxKind, expectedKind As SyntaxKind)
      Report(span, $"Unexpected token <{actualKind}>, expected <{expectedKind}>.")
    End Sub

    Public Sub ReportUndefinedUnaryOperator(span As TextSpan, operatorText As String, operandType As TypeSymbol)
      Report(span, $"Unary operator '{operatorText}' is not defined for type '{operandType}'.")
    End Sub

    Public Sub ReportUndefinedBinaryOperator(span As TextSpan, operatorText As String, leftType As TypeSymbol, rightType As TypeSymbol)
      Report(span, $"Binary operator '{operatorText}' is not defined for type '{leftType}' and '{rightType}'.")
    End Sub

    Public Sub ReportParameterAlreadyDeclared(span As TextSpan, parameterName As String)
      Report(span, $"A parameter with the name '{parameterName}' already exists.")
    End Sub

    'Public Sub ReportUndefinedName(span As TextSpan, name As String)
    Public Sub ReportUndefinedVariable(span As TextSpan, name As String)
      Report(span, $"Variable '{name}' doesn't exist.")
    End Sub

    Public Sub ReportNotAVariable(span As TextSpan, name As String)
      Report(span, $"'{name}' is not a variable.")
    End Sub

    Friend Sub ReportUndefinedType(span As TextSpan, name As String)
      Report(span, $"Type '{name}' doesn't exist.")
    End Sub

    Public Sub ReportCannotConvert(span As TextSpan, fromType As TypeSymbol, toType As TypeSymbol)
      Report(span, $"Cannot convert type '{fromType}' to '{toType}'.")
    End Sub

    Public Sub ReportCannotConvertImplicitly(span As TextSpan, fromType As TypeSymbol, toType As TypeSymbol)
      Report(span, $"Cannot convert type '{fromType}' to '{toType}'. An explicit conversion exists (are you missing a cast?)")
    End Sub

    Public Sub ReportSymbolAlreadyDeclared(span As TextSpan, name As String)
      Report(span, $"'{name}' is already declared.")
    End Sub

    Public Sub ReportCannotAssign(span As TextSpan, name As String)
      Report(span, $"Variable '{name}' is read-only and cannot be assigned to.")
    End Sub

    Public Sub ReportUndefinedFunction(span As TextSpan, name As String)
      Report(span, $"Function '{name}' doesn't exist.")
    End Sub

    Public Sub ReportExpressionMustHaveValue(span As TextSpan)
      Report(span, "Expression must have a value.")
    End Sub

    Public Sub ReportNotAFunction(span As TextSpan, name As String)
      Report(span, $"'{name}' is not a function.")
    End Sub

    Public Sub ReportWrongArgumentCount(span As TextSpan, name As String, expectedCount As Integer, actualCount As Integer)
      Report(span, $"Function '{name}' requires {expectedCount} arguments but was given {actualCount}.")
    End Sub

    Public Sub ReportWrongArgumentType(span As TextSpan, name As String, expectedType As TypeSymbol, actualType As TypeSymbol)
      Report(span, $"Parameter '{name}' requires a value of type '{expectedType}' but was given a value of type '{actualType}'.")
    End Sub

    Public Sub ReportInvalidBreakOrContinue(span As TextSpan, text As String)
      Report(span, $"The keyword '{text}' can only be used inside of loops.")
    End Sub

    Public Sub ReportAllPathsMustReturn(span As TextSpan)
      Report(span, "Not all code paths return a value.")
    End Sub

    Public Sub ReportInvalidReturn(span As TextSpan)
      Report(span, "The 'return' keyword can only be used inside of functions.")
    End Sub

    Public Sub ReportInvalidReturnExpression(span As TextSpan, functionName As String)
      Report(span, $"Since the function '{functionName}' does not return a value, the 'return' keyword cannot be followed by an expression.")
    End Sub

    Public Sub ReportMissingReturnExpression(span As TextSpan, returnType As TypeSymbol)
      Report(span, $"An expression of type '{returnType}' is expected.")
    End Sub

  End Class

End Namespace