Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend MustInherit Class BoundNode

    MustOverride ReadOnly Property Kind As BoundNodeKind

  End Class

End Namespace