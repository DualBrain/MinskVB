Option Explicit On
Option Strict On
Option Infer On

Imports Xunit
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.Tests.CodeAnalysis.Syntax

  Public Class ParserTests

    <Theory>
    <MemberData(NameOf(GetBinaryOperatorPairsData))>
    Public Sub Parser_BinaryExpression_HonorsPrecedences(op1 As SyntaxKind, op2 As SyntaxKind)

      Dim op1Precedence = SyntaxFacts.GetBinaryOperatorPrecedence(op1)
      Dim op2Precedence = SyntaxFacts.GetBinaryOperatorPrecedence(op2)
      Dim op1Text = SyntaxFacts.GetText(op1)
      Dim op2Text = SyntaxFacts.GetText(op2)
      Dim text = $"a {op1Text} b {op2Text} c"
      Dim expression = SyntaxTree.Parse(text).Root

      If op1Precedence >= op2Precedence Then

        '     op2
        '     / \
        '   op1  c
        '   / \
        '  a   b

        Using e = New AssertingEnumerator(expression)
          e.AssertNode(SyntaxKind.BinaryExpression)
          e.AssertNode(SyntaxKind.BinaryExpression)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "a")
          e.AssertToken(op1, op1Text)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "b")
          e.AssertToken(op2, op2Text)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "c")
        End Using

      Else

        '   op1
        '   / \
        '  a  op2
        '     / \
        '    b   c

        Using e = New AssertingEnumerator(expression)
          e.AssertNode(SyntaxKind.BinaryExpression)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "a")
          e.AssertToken(op1, op1Text)
          e.AssertNode(SyntaxKind.BinaryExpression)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "b")
          e.AssertToken(op2, op2Text)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "c")
        End Using

      End If

    End Sub

    Public Shared Iterator Function GetBinaryOperatorPairsData() As IEnumerable(Of Object())
      For Each op1 In SyntaxFacts.GetBinaryOperatorKinds
        For Each op2 In SyntaxFacts.GetBinaryOperatorKinds
          Yield New Object() {op1, op2}
        Next
      Next
    End Function

  End Class

End Namespace