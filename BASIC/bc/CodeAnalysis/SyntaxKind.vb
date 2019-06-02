Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis

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
    BinaryExpression
    ParenExpression

  End Enum

End Namespace