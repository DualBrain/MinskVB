Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis.Binding
  Friend NotInheritable Class BoundBinaryOperator

    Sub New(syntaxKind As SyntaxKind, kind As BoundBinaryOperatorKind, type As Type)
      Me.New(syntaxKind, kind, type, type, type)
    End Sub

    Sub New(syntaxKind As SyntaxKind, kind As BoundBinaryOperatorKind, operandType As Type, resultType As Type)
      Me.New(syntaxKind, kind, operandType, operandType, resultType)
    End Sub

    Sub New(syntaxKind As SyntaxKind, kind As BoundBinaryOperatorKind, leftType As Type, rightType As Type, resultType As Type)
      Me.SyntaxKind = syntaxKind
      Me.Kind = kind
      Me.LeftType = leftType
      Me.RightType = rightType
      Me.Type = resultType
    End Sub

    Public ReadOnly Property SyntaxKind As SyntaxKind
    Public ReadOnly Property Kind As BoundBinaryOperatorKind
    Public ReadOnly Property LeftType As Type
    Public ReadOnly Property RightType As Type
    Public ReadOnly Property Type As Type

    ' New BoundBinaryOperator(SyntaxKind.EqualsToken, BoundBinaryOperatorKind.Equals, GetType(Integer), GetType(Boolean)),
    ' New BoundBinaryOperator(SyntaxKind.EqualsToken, BoundBinaryOperatorKind.Equals, GetType(Boolean)),

    Private Shared ReadOnly m_operators() As BoundBinaryOperator = {
      New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, GetType(Integer)),
      New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, GetType(Integer)),
      New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, GetType(Integer)),
      New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, GetType(Integer)),
      New BoundBinaryOperator(SyntaxKind.AmpersandToken, BoundBinaryOperatorKind.BitwiseAnd, GetType(Integer)),
      New BoundBinaryOperator(SyntaxKind.PipeToken, BoundBinaryOperatorKind.BitwiseOr, GetType(Integer)),
      New BoundBinaryOperator(SyntaxKind.HatToken, BoundBinaryOperatorKind.BitwiseXor, GetType(Integer)),
      New BoundBinaryOperator(SyntaxKind.EqualsEqualsToken, BoundBinaryOperatorKind.Equals, GetType(Integer), GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.BangEqualsToken, BoundBinaryOperatorKind.NotEquals, GetType(Integer), GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.Less, GetType(Integer), GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.LessThanEqualsToken, BoundBinaryOperatorKind.LessOrEquals, GetType(Integer), GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEquals, GetType(Integer), GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.GreaterThanEqualsToken, BoundBinaryOperatorKind.GreaterOrEquals, GetType(Integer), GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.Greater, GetType(Integer), GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.AmpersandToken, BoundBinaryOperatorKind.BitwiseAnd, GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.AmpersandAmpersandToken, BoundBinaryOperatorKind.LogicalAnd, GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.PipeToken, BoundBinaryOperatorKind.BitwiseOr, GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.PipePipeToken, BoundBinaryOperatorKind.LogicalOr, GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.HatToken, BoundBinaryOperatorKind.BitwiseXor, GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.LogicalAnd, GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.LogicalOr, GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.EqualsEqualsToken, BoundBinaryOperatorKind.Equals, GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.BangEqualsToken, BoundBinaryOperatorKind.NotEquals, GetType(Boolean)),
      New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEquals, GetType(Boolean))
    }

    Public Shared Function Bind(syntaxKind As SyntaxKind, leftType As Type, rightType As Type) As BoundBinaryOperator
      For Each op In m_operators
        If op.SyntaxKind = syntaxKind AndAlso op.LeftType Is leftType AndAlso op.RightType Is rightType Then
          Return op
        End If
      Next
      Return Nothing
    End Function

  End Class

End Namespace