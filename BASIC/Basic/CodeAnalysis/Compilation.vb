﻿Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports System.IO
Imports System.Threading

Imports Basic.CodeAnalysis.Binding
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis

  Public NotInheritable Class Compilation

    Private m_globalScope As BoundGlobalScope = Nothing

    Public Sub New(syntax As SyntaxTree)
      Me.New(Nothing, syntax)
    End Sub

    Private Sub New(previous As Compilation, syntax As SyntaxTree)
      Me.Previous = previous
      Me.Syntax = syntax
    End Sub

    Public ReadOnly Property Previous As Compilation
    Public ReadOnly Property Syntax As SyntaxTree

    Friend ReadOnly Property GlobalScope As BoundGlobalScope
      Get
        If Me.m_globalScope Is Nothing Then
          Dim g = Binder.BindGlobalScope(Me.Previous?.GlobalScope, Me.Syntax.Root)
          Interlocked.CompareExchange(Me.m_globalScope, g, Nothing)
        End If
        Return Me.m_globalScope
      End Get
    End Property

    Public Function ContinueWith(syntax As SyntaxTree) As Compilation
      Return New Compilation(Me, syntax)
    End Function

    Public Function Evaluate(variables As Dictionary(Of VariableSymbol, Object)) As EvaluationResult

      Dim diagnostics = Me.Syntax.Diagnostics.Concat(Me.GlobalScope.Diagnostics).ToImmutableArray
      If diagnostics.Any Then
        Return New EvaluationResult(diagnostics, Nothing)
      End If

      Dim program = Binder.BindProgram(Me.GlobalScope)
      If program.Diagnostics.Any Then
        Return New EvaluationResult(program.Diagnostics.ToImmutableArray, Nothing)
      End If

      Dim evaluator = New Evaluator(program, variables)
      Dim value = evaluator.Evaluate

      Return New EvaluationResult(ImmutableArray(Of Diagnostic).Empty, value)

    End Function

    Public Sub EmitTree(writer As TextWriter)
      Dim program = Binder.BindProgram(Me.GlobalScope)
      If program.Statement.Statements.Any() Then
        program.Statement.WriteTo(writer)
      Else
        For Each functionBody In program.Functions
          If Not Me.GlobalScope.Functions.Contains(functionBody.Key) Then
            Continue For
          End If
          functionBody.Key.WriteTo(writer)
          functionBody.Value.WriteTo(writer)
        Next
      End If
    End Sub

  End Class

End Namespace