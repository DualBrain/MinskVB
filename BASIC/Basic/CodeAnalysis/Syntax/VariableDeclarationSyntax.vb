Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  ' var x = 10 ' variable you can assign again
  ' let x = 10 ' variable that is readonly
  ' dim x = 10 ' same as "var"
  Public NotInheritable Class VariableDeclarationSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, keyword As SyntaxToken, identifier As SyntaxToken, typeClause As TypeClauseSyntax, equalsToken As SyntaxToken, initializer As ExpressionSyntax)
      MyBase.New(tree)
      Me.Keyword = keyword
      Me.Identifier = identifier
      Me.TypeClause = typeClause
      Me.EqualsToken = equalsToken
      Me.Initializer = initializer
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.VariableDeclaration
    Public ReadOnly Property Keyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property TypeClause As TypeClauseSyntax
    Public ReadOnly Property EqualsToken As SyntaxToken
    Public ReadOnly Property Initializer As ExpressionSyntax

  End Class

End Namespace