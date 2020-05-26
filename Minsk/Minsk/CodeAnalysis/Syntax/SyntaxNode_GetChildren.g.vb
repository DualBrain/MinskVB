Option Explicit On
Option Strict On
Option Infer On

Imports System.Reflection
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis.Syntax

  Partial Class AssignmentExpressionSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield IdentifierToken
      Yield EqualsToken
      Yield Expression
    End Function

  End Class

  Partial Class BinaryExpressionSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Left
      Yield OperatorToken
      Yield Right
    End Function

  End Class

  Partial Class BlockStatementSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield OpenBraceToken
      For Each child In Statements
        Yield child
      Next
      Yield CloseBraceToken
    End Function

  End Class

  Partial Class BreakStatementSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Keyword
    End Function

  End Class

  Partial Class CallExpressionSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Identifier
      Yield OpenParen
      For Each child In Arguments.GetWithSeparators
        Yield child
      Next
      Yield CloseParen
    End Function

  End Class

  Partial Class CompilationUnitSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      For Each child In Members
        Yield child
      Next
      Yield EndOfFileToken
    End Function

  End Class

  Partial Class ContinueStatementSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Keyword
    End Function

  End Class

  Partial Class DoWhileStatementSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield DoKeyword
      Yield Body
      Yield WhileKeyword
      Yield Condition
    End Function

  End Class

  Partial Class ElseClauseSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield ElseKeyword
      Yield ElseStatement
    End Function

  End Class

  Partial Class ExpressionStatementSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Expression
    End Function

  End Class

  Partial Class ForStatementSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Keyword
      Yield Identifier
      Yield EqualsToken
      Yield LowerBound
      Yield ToKeyword
      Yield UpperBound
      Yield Body
    End Function

  End Class

  Partial Class FunctionDeclarationSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield FunctionKeyword
      Yield Identifier
      Yield OpenParen
      For Each child In Parameters.GetWithSeparators
        Yield child
      Next
      Yield CloseParen
      Yield Type
      Yield Body
    End Function

  End Class

  Partial Class GlobalStatementSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Statement
    End Function

  End Class

  Partial Class IfStatementSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield IfKeyword
      Yield Condition
      Yield ThenStatement
      Yield ElseClause
    End Function

  End Class

  Partial Class LiteralExpressionSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield LiteralToken
    End Function

  End Class

  Partial Class NameExpressionSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield IdentifierToken
    End Function

  End Class

  Partial Class ParameterSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Identifier
      Yield Type
    End Function

  End Class

  Partial Class ParenExpressionSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield OpenParenToken
      Yield Expression
      Yield CloseParenToken
    End Function

  End Class

  Partial Class ReturnStatementSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield ReturnKeyword
      Yield Expression
    End Function

  End Class

  Partial Class TypeClauseSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield ColonToken
      Yield Identifier
    End Function

  End Class

  Partial Class UnaryExpressionSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield OperatorToken
      Yield Operand
    End Function

  End Class

  Partial Class VariableDeclarationSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield Keyword
      Yield Identifier
      Yield TypeClause
      Yield EqualsToken
      Yield Initializer
    End Function

  End Class

  Partial Class WhileStatementSyntax

    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
      Yield WhileKeyword
      Yield Condition
      Yield Body
    End Function

  End Class

End Namespace