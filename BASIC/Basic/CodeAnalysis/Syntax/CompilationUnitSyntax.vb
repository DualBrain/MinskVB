Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable

Namespace Global.Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class CompilationUnitSyntax
    Inherits SyntaxNode

    Sub New(tree As SyntaxTree, members As ImmutableArray(Of MemberSyntax), endOfFileToken As SyntaxToken)
      MyBase.New(tree)
      Me.Members = members
      Me.EndOfFileToken = endOfFileToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CompilationUnit
    Public ReadOnly Property Members As ImmutableArray(Of MemberSyntax)
    Public ReadOnly Property EndOfFileToken As SyntaxToken

  End Class

End Namespace