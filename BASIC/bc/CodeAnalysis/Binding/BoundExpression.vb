Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend MustInherit Class BoundExpression
    Inherits BoundNode

    Public MustOverride ReadOnly Property Type As Type

  End Class

End Namespace