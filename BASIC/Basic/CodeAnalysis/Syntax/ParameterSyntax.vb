﻿Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.CodeAnalysis.Syntax

  Public NotInheritable Class ParameterSyntax
    Inherits SyntaxNode

    Sub New(identifier As SyntaxToken, type As TypeClauseSyntax)
      Me.Identifier = identifier
      Me.Type = type
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.Parameter
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property Type As TypeClauseSyntax

  End Class

End Namespace