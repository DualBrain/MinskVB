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
    TildeToken
    HatToken
    AmpersandToken
    AmpersandAmpersandToken
    EqualsEqualsToken
    BangEqualsToken
    LessThanToken
    LessThanEqualsToken
    LessThanGreaterThanToken
    GreaterThanEqualsToken
    GreaterThanToken
    PipeToken
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

    LetKeyword 'TODO: LET has a different behavior in BASIC.
    VarKeyword
    DimKeyword

    IfKeyword
    'ThenKeyword
    ElseKeyword
    'ElseIfKeyword
    'EndIfKeyword

    WhileKeyword

    ForKeyword
    ToKeyword

    ' Nodes

    CompilationUnit
    ElseClause

    ' Statements

    BlockStatement
    VariableDeclaration
    IfStatement
    WhileStatement
    ForStatement
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