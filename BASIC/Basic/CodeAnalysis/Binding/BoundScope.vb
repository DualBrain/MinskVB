Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports System.Runtime.InteropServices
Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundScope

    Private m_symbols As New Dictionary(Of String, Symbol)

    Public Sub New(parent As BoundScope)
      Me.Parent = parent
    End Sub

    Public ReadOnly Property Parent As BoundScope

    Public Function TryDeclareVariable(variable As VariableSymbol) As Boolean
      Return Me.TryDeclareSymbol(variable)
    End Function

    Private Function TryDeclareSymbol(Of TSymbol As Symbol)(symbol As TSymbol) As Boolean
      If Me.m_symbols Is Nothing Then
        Me.m_symbols = New Dictionary(Of String, Symbol)()
      ElseIf Me.m_symbols.ContainsKey(symbol.Name) Then
        Return False
      End If
      Me.m_symbols.Add(symbol.Name, symbol)
      Return True
    End Function

    Public Function TryLookupVariable(name As String, <Out()> ByRef variable As VariableSymbol) As Boolean
      Return Me.TryLookupSymbol(name, variable)
    End Function

    Public Function GetDeclaredVariables() As ImmutableArray(Of VariableSymbol)
      Return Me.GetDeclaredSymbols(Of VariableSymbol)
    End Function

    Public Function TryDeclareFunction([function] As FunctionSymbol) As Boolean
      Return Me.TryDeclareSymbol([function])
    End Function

    Public Function TryLookupFunction(name As String, <Out()> ByRef [function] As FunctionSymbol) As Boolean
      Return Me.TryLookupSymbol(name, [function])
    End Function

    Private Function TryLookupSymbol(Of TSymbol As Symbol)(name As String, ByRef symbol As TSymbol) As Boolean

      symbol = Nothing

      Dim declaredSymbol As Symbol
      If Me.m_symbols IsNot Nothing AndAlso Me.m_symbols.TryGetValue(name.ToLower, declaredSymbol) Then
        If TypeOf declaredSymbol Is TSymbol Then
          symbol = DirectCast(declaredSymbol, TSymbol)
          Return True
        End If
        Return False
      End If

      If Me.Parent Is Nothing Then
        Return False
      End If

      Return Me.Parent.TryLookupSymbol(name, symbol)

    End Function
    Public Function GetDeclaredFunctions() As ImmutableArray(Of FunctionSymbol)
      Return Me.GetDeclaredSymbols(Of FunctionSymbol)
    End Function

    Private Function GetDeclaredSymbols(Of TSymbol As Symbol)() As ImmutableArray(Of TSymbol)
      If Me.m_symbols Is Nothing Then
        Return ImmutableArray(Of TSymbol).Empty
      End If
      Return Me.m_symbols.Values.OfType(Of TSymbol).ToImmutableArray
    End Function

  End Class

End Namespace