Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundCallExpression
    Inherits BoundExpression

    Sub New([function] As FunctionSymbol, arguments As ImmutableArray(Of BoundExpression))
      Me.Function = [function]
      Me.Arguments = arguments
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.CallExpression
    Public Overrides ReadOnly Property Type As TypeSymbol
      Get
        Return Me.Function.Type
      End Get
    End Property
    Public ReadOnly Property [Function] As FunctionSymbol
    Public ReadOnly Property Arguments As ImmutableArray(Of BoundExpression)

  End Class

End Namespace