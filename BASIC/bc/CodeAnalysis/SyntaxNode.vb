Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis

  MustInherit Class SyntaxNode

    Public MustOverride ReadOnly Property Kind() As SyntaxKind

    Public MustOverride Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)

  End Class

End Namespace