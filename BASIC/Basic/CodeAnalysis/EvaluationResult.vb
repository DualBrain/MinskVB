Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis
  Public NotInheritable Class EvaluationResult

    Sub New(diagnostics As IEnumerable(Of Diagnostic), value As Object)
      Me.Diagnostics = diagnostics.ToArray
      Me.Value = value
    End Sub

    Public ReadOnly Property Diagnostics As IReadOnlyList(Of Diagnostic)
    Public ReadOnly Property Value As Object

  End Class

End Namespace