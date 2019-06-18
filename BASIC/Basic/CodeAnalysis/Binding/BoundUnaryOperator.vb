Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundUnaryOperator

    Sub New(syntaxKind As SyntaxKind, kind As BoundUnaryOperatorKind, operatorType As TypeSymbol)
      Me.New(syntaxKind, kind, operatorType, operatorType)
    End Sub

    Sub New(syntaxKind As SyntaxKind, kind As BoundUnaryOperatorKind, operandType As TypeSymbol, resultType As TypeSymbol)
      Me.SyntaxKind = syntaxKind
      Me.Kind = kind
      Me.OperandType = operandType
      Me.Type = resultType
    End Sub

    Public ReadOnly Property SyntaxKind As SyntaxKind
    Public ReadOnly Property Kind As BoundUnaryOperatorKind
    Public ReadOnly Property OperandType As TypeSymbol
    Public ReadOnly Property Type As TypeSymbol

    Private Shared ReadOnly m_operators() As BoundUnaryOperator = {
          New BoundUnaryOperator(SyntaxKind.BangToken, BoundUnaryOperatorKind.LogicalNegation, TypeSymbol.Bool),
          New BoundUnaryOperator(SyntaxKind.NotKeyword, BoundUnaryOperatorKind.LogicalNegation, TypeSymbol.Bool),
          New BoundUnaryOperator(SyntaxKind.PlusToken, BoundUnaryOperatorKind.Identity, TypeSymbol.Int),
          New BoundUnaryOperator(SyntaxKind.MinusToken, BoundUnaryOperatorKind.Negation, TypeSymbol.Int),
          New BoundUnaryOperator(SyntaxKind.TildeToken, BoundUnaryOperatorKind.Onescomplement, TypeSymbol.Int)
        }

    Public Shared Function Bind(syntaxKind As SyntaxKind, operandType As TypeSymbol) As BoundUnaryOperator
      For Each op In m_operators
        If op.SyntaxKind = syntaxKind AndAlso op.OperandType Is operandType Then
          Return op
        End If
      Next
      Return Nothing
    End Function

  End Class

End Namespace