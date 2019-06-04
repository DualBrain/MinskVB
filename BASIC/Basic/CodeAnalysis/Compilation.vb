Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Binding
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis
  Public NotInheritable Class Compilation

    Sub New(syntax As SyntaxTree)
      Me.Syntax = syntax
    End Sub

    Public ReadOnly Property Syntax As SyntaxTree

    Public Function Evaluate(variables As Dictionary(Of String, Object)) As EvaluationResult

      Dim binder = New Binder(variables)
      Dim boundExpression = binder.BindExpression(Me.Syntax.Root)

      Dim diagnostics = Me.Syntax.Diagnostics.Concat(binder.Diagnostics).ToArray

      If diagnostics.Any Then
        Return New EvaluationResult(diagnostics, Nothing)
      End If

      Dim evaluator = New Evaluator(boundExpression, variables)
      Dim value = evaluator.Evaluate

      Return New EvaluationResult(Array.Empty(Of Diagnostic), value)

    End Function

  End Class

End Namespace