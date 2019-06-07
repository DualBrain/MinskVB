Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundBlockStatement
    Inherits BoundStatement

    Sub New(statements As ImmutableArray(Of BoundStatement))
      Me.Statements = statements
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.BlockStatement
    Public ReadOnly Property Statements As ImmutableArray(Of BoundStatement)

  End Class

End Namespace