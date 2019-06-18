Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports System.Reflection

Namespace Global.Basic.CodeAnalysis.Symbols

  Friend Module BuiltinFunctions

    Public ReadOnly Print As New FunctionSymbol("print", ImmutableArray.Create(New ParameterSymbol("text", TypeSymbol.String)), TypeSymbol.Void)
    Public ReadOnly Input As New FunctionSymbol("input", ImmutableArray(Of ParameterSymbol).Empty, TypeSymbol.String)
    Public ReadOnly Rnd As New FunctionSymbol("rnd", ImmutableArray.Create(New ParameterSymbol("max", TypeSymbol.Int)), TypeSymbol.Int)

    Friend Function GetAll() As IEnumerable(Of FunctionSymbol)
      Return GetType(BuiltinFunctions).GetFields(BindingFlags.Public Or BindingFlags.Static).
                                       Where(Function(f) f.FieldType = GetType(FunctionSymbol)).
                                       Select(Function(f) CType(f.GetValue(Nothing), FunctionSymbol))
    End Function

  End Module

  Public NotInheritable Class FunctionSymbol
    Inherits Symbol

    Sub New(name As String, paremeters As ImmutableArray(Of ParameterSymbol), type As TypeSymbol)
      MyBase.New(name)
      Me.Parameters = paremeters
      Me.Type = type
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Function
    Public ReadOnly Property Parameters As ImmutableArray(Of ParameterSymbol)
    Public ReadOnly Property Type As TypeSymbol

  End Class

End Namespace