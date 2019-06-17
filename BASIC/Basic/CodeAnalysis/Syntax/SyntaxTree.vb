Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.CodeAnalysis.Syntax

  Public NotInheritable Class SyntaxTree

    Private Sub New(text As SourceText)

      Dim parser = New Parser(text)
      Dim root = parser.ParseCompilationUnit()

      Me.Text = text
      Me.Diagnostics = parser.Diagnostics.ToImmutableArray
      Me.Root = root

    End Sub

    Public ReadOnly Property Text As SourceText
    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Root As CompilationUnitSyntax

    Public Shared Function Parse(text As String) As SyntaxTree
      Dim source = SourceText.From(text)
      Return Parse(source)
    End Function

    Public Shared Function Parse(text As SourceText) As SyntaxTree
      Return New SyntaxTree(text)
    End Function

    Public Shared Function ParseTokens(text As String) As ImmutableArray(Of SyntaxToken)
      Dim source = SourceText.From(text)
      Return ParseTokens(source)
    End Function

    Public Shared Function ParseTokens(text As String, ByRef diagnostics As ImmutableArray(Of Diagnostic)) As ImmutableArray(Of SyntaxToken)
      Dim source = SourceText.From(text)
      Return ParseTokens(source, diagnostics)
    End Function

    Public Shared Function ParseTokens(text As SourceText) As ImmutableArray(Of SyntaxToken)
      Return ParseTokens(text, Nothing)
    End Function

    Public Shared Function ParseTokens(text As SourceText, ByRef diagnostics As ImmutableArray(Of Diagnostic)) As ImmutableArray(Of SyntaxToken)
      Dim lexer = New Lexer(text)
      Dim l = New Lexer(text)
      Dim result = LexTokens(l).ToImmutableArray
      diagnostics = l.Diagnostics.ToImmutableArray
      Return result
    End Function

    Private Shared Iterator Function LexTokens(lexer As Lexer) As IEnumerable(Of SyntaxToken)
      Do
        Dim token = lexer.Lex
        If token.Kind = SyntaxKind.EndOfFileToken Then
          Exit Do
        End If
        Yield token
      Loop
    End Function

  End Class

End Namespace