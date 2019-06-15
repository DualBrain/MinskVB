Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend Enum BoundBinaryOperatorKind
    Addition
    Subtraction
    Multiplication
    Division
    LogicalAnd
    LogicalOr
    Equals
    NotEquals
    Less
    LessOrEquals
    GreaterOrEquals
    Greater
  End Enum

End Namespace