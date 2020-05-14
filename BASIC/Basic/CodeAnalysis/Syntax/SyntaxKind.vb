Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public Enum SyntaxKind

    ' Tokens
    BadToken
    EndOfFileToken
    WhitespaceToken
    SingleLineCommentToken
    MultiLineCommentToken
    NumberToken
    StringToken
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
    'LessThanGreaterThanToken
    GreaterThanEqualsToken
    GreaterThanToken
    PipeToken
    PipePipeToken
    OpenParenToken
    CloseParenToken
    OpenBraceToken
    CloseBraceToken
    ColonToken
    CommaToken
    IdentifierToken

    ' Keywords

    FalseKeyword
    TrueKeyword

    'NotKeyword
    'AndKeyword
    'AndAlsoKeyword
    'OrKeyword
    'OrElseKeyword

    LetKeyword 'TODO: LET has a different behavior in BASIC.
    ReturnKeyword
    VarKeyword
    'DimKeyword

    FunctionKeyword

    IfKeyword
    'ThenKeyword
    BreakKeyword
    ContinueKeyword
    ElseKeyword
    'ElseIfKeyword
    'EndIfKeyword

    WhileKeyword
    DoKeyword

    ForKeyword
    ToKeyword

    ' Nodes

    CompilationUnit
    GlobalStatement
    FunctionDeclaration
    Parameter
    ElseClause
    TypeClause

    ' Statements

    BlockStatement
    VariableDeclaration
    IfStatement
    WhileStatement
    DoWhileStatement
    ForStatement
    BreakStatement
    ContinueStatement
    ReturnStatement
    ExpressionStatement

    ' Expressions

    LiteralExpression
    NameExpression
    UnaryExpression
    BinaryExpression
    ParenExpression
    AssignmentExpression
    CallExpression

  End Enum

End Namespace