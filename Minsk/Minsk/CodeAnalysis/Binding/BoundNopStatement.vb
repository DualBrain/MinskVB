Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding
  Friend NotInheritable Class BoundNopStatement
    Inherits BoundStatement

    Public Overrides ReadOnly Property Kind As BoundNodeKind
      Get
        Return BoundNodeKind.NopStatement
      End Get
    End Property

  End Class


End Namespace