Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundProgram

    Public Sub New(globalScope As BoundGlobalScope, diagnostics As DiagnosticBag, functionBodies As ImmutableDictionary(Of FunctionSymbol, BoundBlockStatement))
      Me.GlobalScope = globalScope
      Me.Diagnostics = diagnostics
      Me.FunctionBodies = functionBodies
    End Sub

    Public ReadOnly Property GlobalScope As BoundGlobalScope
    Public ReadOnly Property Diagnostics As DiagnosticBag
    Public ReadOnly Property FunctionBodies As ImmutableDictionary(Of FunctionSymbol, BoundBlockStatement)

  End Class

End Namespace