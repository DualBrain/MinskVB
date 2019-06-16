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