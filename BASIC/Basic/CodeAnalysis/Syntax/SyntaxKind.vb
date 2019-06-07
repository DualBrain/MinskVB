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
    BangToken
    EqualsToken
    AmpersandAmpersandToken
    EqualsEqualsToken
    BangEqualsToken
    LessThanGreaterThanToken
    PipePipeToken
    OpenParenToken
    CloseParenToken
    OpenBraceToken
    CloseBraceToken
    IdentifierToken

    ' Keywords

    FalseKeyword
    TrueKeyword
    NotKeyword
    AndKeyword
    AndAlsoKeyword
    OrKeyword
    OrElseKeyword

    ' Nodes

    CompilationUnit

    ' Statements

    BlockStatement
    ExpressionStatement

    ' Expressions

    LiteralExpression
    NameExpression
    UnaryExpression
    BinaryExpression
    ParenExpression
    AssignmentExpression

  End Enum

End Namespace