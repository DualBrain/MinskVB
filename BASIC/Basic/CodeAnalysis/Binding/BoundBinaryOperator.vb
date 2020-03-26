Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundBinaryOperator

    Sub New(syntaxKind As SyntaxKind, kind As BoundBinaryOperatorKind, type As TypeSymbol)
      Me.New(syntaxKind, kind, type, type, type)
    End Sub

    Sub New(syntaxKind As SyntaxKind, kind As BoundBinaryOperatorKind, operandType As TypeSymbol, resultType As TypeSymbol)
      Me.New(syntaxKind, kind, operandType, operandType, resultType)
    End Sub

    Sub New(syntaxKind As SyntaxKind, kind As BoundBinaryOperatorKind, leftType As TypeSymbol, rightType As TypeSymbol, resultType As TypeSymbol)
      Me.SyntaxKind = syntaxKind
      Me.Kind = kind
      Me.LeftType = leftType
      Me.RightType = rightType
      Type = resultType
    End Sub

    Public ReadOnly Property SyntaxKind As SyntaxKind
    Public ReadOnly Property Kind As BoundBinaryOperatorKind
    Public ReadOnly Property LeftType As TypeSymbol
    Public ReadOnly Property RightType As TypeSymbol
    Public ReadOnly Property Type As TypeSymbol

    Private Shared ReadOnly m_operators() As BoundBinaryOperator = {
          New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.Int),
          New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.Int),
          New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.Int),
          New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.Int),
          New BoundBinaryOperator(SyntaxKind.AmpersandToken, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.Int),
          New BoundBinaryOperator(SyntaxKind.PipeToken, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.Int),
          New BoundBinaryOperator(SyntaxKind.HatToken, BoundBinaryOperatorKind.BitwiseXor, TypeSymbol.Int),
          New BoundBinaryOperator(SyntaxKind.EqualsEqualsToken, BoundBinaryOperatorKind.Equals, TypeSymbol.Int, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.BangEqualsToken, BoundBinaryOperatorKind.NotEquals, TypeSymbol.Int, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.Less, TypeSymbol.Int, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.LessThanEqualsToken, BoundBinaryOperatorKind.LessOrEquals, TypeSymbol.Int, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEquals, TypeSymbol.Int, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.GreaterThanEqualsToken, BoundBinaryOperatorKind.GreaterOrEquals, TypeSymbol.Int, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.Greater, TypeSymbol.Int, TypeSymbol.Bool),
 _
          New BoundBinaryOperator(SyntaxKind.AmpersandToken, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.AmpersandAmpersandToken, BoundBinaryOperatorKind.LogicalAnd, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.PipeToken, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.PipePipeToken, BoundBinaryOperatorKind.LogicalOr, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.HatToken, BoundBinaryOperatorKind.BitwiseXor, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.LogicalAnd, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.LogicalOr, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.EqualsEqualsToken, BoundBinaryOperatorKind.Equals, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEquals, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.BangEqualsToken, BoundBinaryOperatorKind.NotEquals, TypeSymbol.Bool),
 _
          New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.String),
          New BoundBinaryOperator(SyntaxKind.EqualsEqualsToken, BoundBinaryOperatorKind.Equals, TypeSymbol.String, TypeSymbol.Bool),
          New BoundBinaryOperator(SyntaxKind.BangEqualsToken, BoundBinaryOperatorKind.NotEquals, TypeSymbol.String, TypeSymbol.Bool)
        }

    Public Shared Function Bind(syntaxKind As SyntaxKind, leftType As TypeSymbol, rightType As TypeSymbol) As BoundBinaryOperator
      For Each op In m_operators
        If op.SyntaxKind = syntaxKind AndAlso op.LeftType Is leftType AndAlso op.RightType Is rightType Then
          Return op
        End If
      Next
      Return Nothing
    End Function

  End Class

End Namespace