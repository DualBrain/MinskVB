Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Binding

  Friend Enum BoundNodeKind

    ' Statements
    BlockStatement
    VariableDeclaration
    IfStatement
    WhileStatement
    DoWhileStatement
    ForStatement
    LabelStatement
    GotoStatement
    ConditionalGotoStatement
    ExpressionStatement

    ' Expressions
    ErrorExpression
    LiteralExpression
    VariableExpression
    AssignmentExpression
    UnaryExpression
    BinaryExpression
    CallExpression
    ConversionExpression

  End Enum

End Namespace