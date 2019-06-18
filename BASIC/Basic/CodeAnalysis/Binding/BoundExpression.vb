Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Symbols

Namespace Global.Basic.CodeAnalysis.Binding

  Friend MustInherit Class BoundExpression
    Inherits BoundNode

    Public MustOverride ReadOnly Property Type As TypeSymbol

  End Class

End Namespace