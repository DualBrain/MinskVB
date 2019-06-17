Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports System.Runtime.InteropServices

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundScope

    Private ReadOnly m_variables As Dictionary(Of String, VariableSymbol) = New Dictionary(Of String, VariableSymbol)

    Public Sub New(parent As BoundScope)
      Me.Parent = parent
    End Sub

    Public ReadOnly Property Parent As BoundScope

    Public Function TryDeclare(variable As VariableSymbol) As Boolean

      ' var x = 10
      ' {
      '   var x = false
      ' }

      If Me.m_variables.ContainsKey(variable.Name.ToLower) Then
        Return False
      End If

      Me.m_variables.Add(variable.Name.ToLower, variable)
      Return True

    End Function

    Public Function TryLookup(name As String, <Out()> ByRef variable As VariableSymbol) As Boolean

      If Me.m_variables.TryGetValue(name.ToLower, variable) Then
        Return True
      End If

      If Me.Parent Is Nothing Then
        Return False
      End If

      Return Me.Parent.TryLookup(name.ToLower, variable)

    End Function

    Public Function GetDeclaredVariables() As ImmutableArray(Of VariableSymbol)
      Return Me.m_variables.Values.ToImmutableArray
    End Function

  End Class

End Namespace