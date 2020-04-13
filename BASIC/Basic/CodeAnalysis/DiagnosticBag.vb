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

    Private Sub Report(location As TextLocation, message As String)
      Dim diagnostic = New Diagnostic(location, message)
      m_diagnostics.Add(diagnostic)
    End Sub

    Public Sub AddRange(diagnostics As DiagnosticBag)
      m_diagnostics.AddRange(diagnostics.m_diagnostics)
    End Sub

    Public Sub Concat(diagnostics As DiagnosticBag)
      m_diagnostics.Concat(diagnostics.m_diagnostics)
    End Sub

    Public Sub ReportInvalidNumber(location As TextLocation, text As String, type As TypeSymbol)
      Report(location, $"The number {text} isn't valid {type}.")
    End Sub

    Public Sub ReportBadCharacter(location As TextLocation, character As Char)
      Report(location, $"Bad character input: '{character}'.")
    End Sub

    Public Sub ReportUnterminatedString(location As TextLocation)
      Report(location, $"Unterminated string literal.")
    End Sub

    Public Sub ReportUnexpectedToken(location As TextLocation, actualKind As SyntaxKind, expectedKind As SyntaxKind)
      Report(location, $"Unexpected token <{actualKind}>, expected <{expectedKind}>.")
    End Sub

    Public Sub ReportUndefinedUnaryOperator(location As TextLocation, operatorText As String, operandType As TypeSymbol)
      Report(location, $"Unary operator '{operatorText}' is not defined for type '{operandType}'.")
    End Sub

    Public Sub ReportUndefinedBinaryOperator(location As TextLocation, operatorText As String, leftType As TypeSymbol, rightType As TypeSymbol)
      Report(location, $"Binary operator '{operatorText}' is not defined for type '{leftType}' and '{rightType}'.")
    End Sub

    Public Sub ReportParameterAlreadyDeclared(location As TextLocation, parameterName As String)
      Report(location, $"A parameter with the name '{parameterName}' already exists.")
    End Sub

    'Public Sub ReportUndefinedName(location As TextLocation, name As String)
    Public Sub ReportUndefinedVariable(location As TextLocation, name As String)
      Report(location, $"Variable '{name}' doesn't exist.")
    End Sub

    Public Sub ReportNotAVariable(location As TextLocation, name As String)
      Report(location, $"'{name}' is not a variable.")
    End Sub

    Friend Sub ReportUndefinedType(location As TextLocation, name As String)
      Report(location, $"Type '{name}' doesn't exist.")
    End Sub

    Public Sub ReportCannotConvert(location As TextLocation, fromType As TypeSymbol, toType As TypeSymbol)
      Report(location, $"Cannot convert type '{fromType}' to '{toType}'.")
    End Sub

    Public Sub ReportCannotConvertImplicitly(location As TextLocation, fromType As TypeSymbol, toType As TypeSymbol)
      Report(location, $"Cannot convert type '{fromType}' to '{toType}'. An explicit conversion exists (are you missing a cast?)")
    End Sub

    Public Sub ReportSymbolAlreadyDeclared(location As TextLocation, name As String)
      Report(location, $"'{name}' is already declared.")
    End Sub

    Public Sub ReportCannotAssign(location As TextLocation, name As String)
      Report(location, $"Variable '{name}' is read-only and cannot be assigned to.")
    End Sub

    Public Sub ReportUndefinedFunction(location As TextLocation, name As String)
      Report(location, $"Function '{name}' doesn't exist.")
    End Sub

    Public Sub ReportExpressionMustHaveValue(location As TextLocation)
      Report(location, "Expression must have a value.")
    End Sub

    Public Sub ReportNotAFunction(location As TextLocation, name As String)
      Report(location, $"'{name}' is not a function.")
    End Sub

    Public Sub ReportWrongArgumentCount(location As TextLocation, name As String, expectedCount As Integer, actualCount As Integer)
      Report(location, $"Function '{name}' requires {expectedCount} arguments but was given {actualCount}.")
    End Sub

    Public Sub ReportWrongArgumentType(location As TextLocation, name As String, expectedType As TypeSymbol, actualType As TypeSymbol)
      Report(location, $"Parameter '{name}' requires a value of type '{expectedType}' but was given a value of type '{actualType}'.")
    End Sub

    Public Sub ReportInvalidBreakOrContinue(location As TextLocation, text As String)
      Report(location, $"The keyword '{text}' can only be used inside of loops.")
    End Sub

    Public Sub ReportAllPathsMustReturn(location As TextLocation)
      Report(location, "Not all code paths return a value.")
    End Sub

    Public Sub ReportInvalidReturnExpression(location As TextLocation, functionName As String)
      Report(location, $"Since the function '{functionName}' does not return a value, the 'return' keyword cannot be followed by an expression.")
    End Sub

    Public Sub ReportMissingReturnExpression(location As TextLocation, returnType As TypeSymbol)
      Report(location, $"An expression of type '{returnType}' is expected.")
    End Sub

    Public Sub ReportInvalidExpressionStatement(location As TextLocation)
      Report(location, "Only assignment and call epxressions can be used as a statement.")
    End Sub

    Public Sub ReportOnlyOneFileCanHaveGlobalStatements(location As TextLocation)
      Report(location, "At most one file can have global statements.")
    End Sub

    Public Sub ReportMainMustHaveCorrectSignature(location As TextLocation)
      Report(location, "main must not take arguments and not return anything.")
    End Sub

    Public Sub ReportCannotMixMainAndGlobalStatements(location As TextLocation)
      Report(location, "Cannot declare main function when global statements are used.")
    End Sub

  End Class

End Namespace