Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports System.Runtime.InteropServices
Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundScope

    Private m_variables As Dictionary(Of String, VariableSymbol)
    Private m_functions As Dictionary(Of String, FunctionSymbol) = New Dictionary(Of String, FunctionSymbol)

    Public Sub New(parent As BoundScope)
      Me.Parent = parent
    End Sub

    Public ReadOnly Property Parent As BoundScope

    Public Function TryDeclareVariable(variable As VariableSymbol) As Boolean

      If Me.m_variables Is Nothing Then
        Me.m_variables = New Dictionary(Of String, VariableSymbol)
      End If

      If Me.m_variables.ContainsKey(variable?.Name?.ToLower) Then
        Return False
      End If

      Me.m_variables.Add(variable?.Name?.ToLower, variable)
      Return True

    End Function

    Public Function TryLookupVariable(name As String, <Out()> ByRef variable As VariableSymbol) As Boolean

      variable = Nothing

      If Me.m_variables IsNot Nothing AndAlso Me.m_variables.TryGetValue(name.ToLower, variable) Then
        Return True
      End If

      If Me.Parent Is Nothing Then
        Return False
      End If

      Return Me.Parent.TryLookupVariable(name.ToLower, variable)

    End Function

    Public Function GetDeclaredVariables() As ImmutableArray(Of VariableSymbol)
      If Me.m_variables Is Nothing Then
        Return ImmutableArray(Of VariableSymbol).Empty
      End If
      Return Me.m_variables.Values.ToImmutableArray
    End Function

    Public Function TryDeclareFunction([function] As FunctionSymbol) As Boolean

      If Me.m_functions Is Nothing Then
        Me.m_functions = New Dictionary(Of String, FunctionSymbol)
      End If

      If Me.m_functions.ContainsKey([function]?.Name?.ToLower) Then
        Return False
      End If

      Me.m_functions.Add([function]?.Name?.ToLower, [function])
      Return True

    End Function

    Public Function TryLookupFunction(name As String, <Out()> ByRef [function] As FunctionSymbol) As Boolean

      [function] = Nothing

      If Me.m_functions IsNot Nothing AndAlso Me.m_functions.TryGetValue(name.ToLower, [function]) Then
        Return True
      End If

      If Me.Parent Is Nothing Then
        Return False
      End If

      Return Me.Parent.TryLookupFunction(name.ToLower, [function])

    End Function

    Public Function GetDeclaredFunctions() As ImmutableArray(Of FunctionSymbol)
      If Me.m_functions Is Nothing Then
        Return ImmutableArray(Of FunctionSymbol).Empty
      End If
      Return Me.m_functions.Values.ToImmutableArray
    End Function

  End Class

End Namespace