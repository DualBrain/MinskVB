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
    ForStatement
    LabelStatement
    GotoStatement
    ConditionalGotoStatement
    ExpressionStatement

    ' Expressions
    LiteralExpression
    VariableExpression
    AssignmentExpression
    UnaryExpression
    BinaryExpression

  End Enum

End Namespace