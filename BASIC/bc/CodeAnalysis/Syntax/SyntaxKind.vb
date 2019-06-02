Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public Enum SyntaxKind

    ' Tokens
    BadToken
    EndOfFileToken
    WhitespaceToken
    NumberToken
    PlusToken
    MinusToken
    StarToken
    SlashToken
    OpenParenToken
    CloseParenToken

    ' Expressions

    LiteralExpression
    UnaryExpression
    BinaryExpression
    ParenExpression

  End Enum

End Namespace