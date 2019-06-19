Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Symbols

  Public Enum SymbolKind
    [Function]
    GlobalVariable
    LocalVariable
    Parameter
    Type
  End Enum

End Namespace