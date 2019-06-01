Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis

  Enum SyntaxKind
    NumberToken
    WhitespaceToken
    PlusToken
    MinusToken
    StarToken
    SlashToken
    OpenParenToken
    CloseParenToken
    BadToken
    EndOfFileToken
    BinaryExpression
    NumberExpression
    ParenExpression
  End Enum

End Namespace