Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public Enum SyntaxKind

    BadToken

    ' Trivia
    SkippedTextTrivia
    LineBreakTrivia
    WhitespaceTrivia
    SingleLineCommentTrivia
    MultiLineCommentTrivia

    ' Tokens
    EndOfFileToken
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
    DimKeyword

    AsKeyword
    BooleanKeyword
    ShortKeyword
    IntegerKeyword
    LongKeyword

    SubKeyword
    FunctionKeyword

    ByRefKeyword
    ByValKeyword
    OptionalKeyword

    IfKeyword
    ThenKeyword
    BreakKeyword
    ElseKeyword
    ElseIfKeyword
    EndKeyword

    WhileKeyword
    WendKeyword
    DoKeyword
    LoopKeyword

    ForKeyword
    ToKeyword
    StepKeyword
    NextKeyword
    ExitKeyword
    EachKeyword
    InKeyword
    ContinueKeyword

    PrintKeyword

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