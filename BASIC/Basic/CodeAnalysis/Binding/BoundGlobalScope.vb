﻿Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundGlobalScope

    Sub New(previous As BoundGlobalScope, diagnostics As ImmutableArray(Of Diagnostic), functions As ImmutableArray(Of FunctionSymbol), variables As ImmutableArray(Of VariableSymbol), statements As ImmutableArray(Of BoundStatement))
      Me.Previous = previous
      Me.Diagnostics = diagnostics
      Me.Functions = functions
      Me.Variables = variables
      Me.Statements = statements
    End Sub

    Public ReadOnly Property Previous As BoundGlobalScope
    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Functions As ImmutableArray(Of FunctionSymbol)
    Public ReadOnly Property Variables As ImmutableArray(Of VariableSymbol)
    Public ReadOnly Property Statements As ImmutableArray(Of BoundStatement)

  End Class

End Namespace