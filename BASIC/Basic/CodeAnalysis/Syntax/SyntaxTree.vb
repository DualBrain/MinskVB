Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.CodeAnalysis.Syntax

  Public NotInheritable Class SyntaxTree

    Sub New(text As SourceText, diagnostics As ImmutableArray(Of Diagnostic), root As ExpressionSyntax, endOfFileToken As SyntaxToken)
      Me.Text = text
      Me.Diagnostics = diagnostics
      Me.Root = root
      Me.EndOfFileToken = endOfFileToken
    End Sub

    Public ReadOnly Property Text As SourceText
    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Root As ExpressionSyntax
    Public ReadOnly Property EndOfFileToken As SyntaxToken

    Public Shared Function Parse(text As String) As SyntaxTree
      Dim source = SourceText.From(text)
      Return Parse(source)
    End Function

    Public Shared Function Parse(text As SourceText) As SyntaxTree
      Dim parser = New Parser(text)
      Return parser.Parse
    End Function

    Public Shared Function ParseTokens(Text As String) As IEnumerable(Of SyntaxToken)
      Dim source = SourceText.From(Text)
      Return ParseTokens(source)
    End Function

    Public Shared Iterator Function ParseTokens(Text As SourceText) As IEnumerable(Of SyntaxToken)
      Dim lexer = New Lexer(Text)
      While True
        Dim token = lexer.Lex
        If token.Kind = SyntaxKind.EndOfFileToken Then
          Exit While
        End If
        Yield token
      End While
    End Function

  End Class

End Namespace