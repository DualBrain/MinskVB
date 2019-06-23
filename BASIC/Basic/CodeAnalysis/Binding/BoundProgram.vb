Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundProgram

    Public Sub New(diagnostics As ImmutableArray(Of Diagnostic), functions As ImmutableDictionary(Of FunctionSymbol, BoundBlockStatement), statement As BoundBlockStatement)
      Me.Diagnostics = diagnostics
      Me.Functions = functions
      Me.Statement = statement
    End Sub

    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Functions As ImmutableDictionary(Of FunctionSymbol, BoundBlockStatement)
    Public ReadOnly Property Statement As BoundBlockStatement
  End Class

End Namespace