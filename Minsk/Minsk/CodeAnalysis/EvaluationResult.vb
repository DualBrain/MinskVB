Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable

Namespace Global.Basic.CodeAnalysis
  Public NotInheritable Class EvaluationResult

    Sub New(diagnostics As ImmutableArray(Of Diagnostic), value As Object)
      Me.Diagnostics = diagnostics
      Me.Value = value
      ErrorDiagnostics = diagnostics.Where(Function(d) d.IsError).ToImmutableArray
      WarningDiagnostics = diagnostics.Where(Function(d) d.IsWarning).ToImmutableArray
    End Sub

    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property ErrorDiagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property WarningDiagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Value As Object

  End Class
End Namespace