Option Explicit On
Option Strict On
Option Infer On

Imports Xunit
Imports Basic.CodeAnalysis.Syntax
Imports System.Collections.Generic

Namespace Global.Basic.Tests.CodeAnalysis.Syntax

  Public Class LexerTest

    <Theory>
    <MemberData(NameOf(GetTokensData))>
    Sub Lexer_Lexes_Token(kind As SyntaxKind, text As String)

      Dim tokens = SyntaxTree.ParseTokens(text)

      Dim token = Assert.Single(tokens)
      Assert.Equal(kind, token.Kind)
      Assert.Equal(text, token.Text)

    End Sub

    <Theory>
    <MemberData(NameOf(GetTokenPairsData))>
    Sub Lexer_Lexes_TokenPair(kind1 As SyntaxKind, text1 As String, kind2 As SyntaxKind, text2 As String)

      Dim text = text1 & text2
      Dim tokens = SyntaxTree.ParseTokens(text).ToArray

      Assert.Equal(2, tokens.Length)
      Assert.Equal(tokens(0).Kind, kind1)
      Assert.Equal(tokens(0).Text, text1)
      Assert.Equal(tokens(1).Kind, kind2)
      Assert.Equal(tokens(1).Text, text2)

    End Sub

    <Theory>
    <MemberData(NameOf(GetTokenPairsWithSeparatorData))>
    Sub Lexer_Lexes_TokenPairWithSeparator(kind1 As SyntaxKind, text1 As String,
                                           separatorKind As SyntaxKind, separatorText As String,
                                           kind2 As SyntaxKind, text2 As String)

      Dim text = text1 & separatorText & text2
      Dim tokens = SyntaxTree.ParseTokens(text).ToArray

      Assert.Equal(3, tokens.Length)
      Assert.Equal(tokens(0).Kind, kind1)
      Assert.Equal(tokens(0).Text, text1)
      Assert.Equal(tokens(1).Kind, separatorKind)
      Assert.Equal(tokens(1).Text, separatorText)
      Assert.Equal(tokens(2).Kind, kind2)
      Assert.Equal(tokens(2).Text, text2)

    End Sub

    Public Shared Iterator Function GetTokensData() As IEnumerable(Of Object)
      For Each t In GetTokens.Concat(GetSeparators)
        Yield New Object() {t.kind, t.text}
      Next
    End Function

    Public Shared Iterator Function GetTokenPairsData() As IEnumerable(Of Object)
      For Each t In GetTokenPairs()
        Yield New Object() {t.kind1, t.text1, t.kind2, t.text2}
      Next
    End Function

    Public Shared Iterator Function GetTokenPairsWithSeparatorData() As IEnumerable(Of Object)
      For Each t In GetTokenPairsWithSeparator()
        Yield New Object() {t.kind1, t.text1, t.separatorKind, t.separatorText, t.kind2, t.text2}
      Next
    End Function

    Private Shared Function GetTokens() As (kind As SyntaxKind, text As String)()

      Return {(SyntaxKind.PlusToken, "+"),
              (SyntaxKind.MinusToken, "-"),
              (SyntaxKind.StarToken, "*"),
              (SyntaxKind.SlashToken, "/"),
              (SyntaxKind.BangToken, "!"),
              (SyntaxKind.EqualsToken, "="),
              (SyntaxKind.AmpersandAmpersandToken, "&&"),
              (SyntaxKind.EqualsEqualsToken, "=="),
              (SyntaxKind.BangEqualsToken, "!="),
              (SyntaxKind.LessThanGreaterThanToken, "<>"),
              (SyntaxKind.PipePipeToken, "||"),
              (SyntaxKind.OpenParenToken, "("),
              (SyntaxKind.CloseParenToken, ")"),
              (SyntaxKind.FalseKeyword, "false"),
              (SyntaxKind.TrueKeyword, "true"),
              (SyntaxKind.NotKeyword, "not"),
              (SyntaxKind.AndKeyword, "and"),
              (SyntaxKind.AndAlsoKeyword, "andalso"),
              (SyntaxKind.OrKeyword, "or"),
              (SyntaxKind.OrElseKeyword, "orelse"),
              (SyntaxKind.NumberToken, "1"),
              (SyntaxKind.NumberToken, "123"),
              (SyntaxKind.IdentifierToken, "a"),
              (SyntaxKind.IdentifierToken, "abc")}

    End Function

    Private Shared Function GetSeparators() As (kind As SyntaxKind, text As String)()

      Return {(SyntaxKind.WhitespaceToken, " "),
              (SyntaxKind.WhitespaceToken, "  "),
              (SyntaxKind.WhitespaceToken, vbCr),
              (SyntaxKind.WhitespaceToken, vbLf),
              (SyntaxKind.WhitespaceToken, vbCrLf)}

    End Function

    Private Shared Function RequiresSeparator(kind1 As SyntaxKind, kind2 As SyntaxKind) As Boolean

      Dim isKeyword1 = kind1.ToString.EndsWith("Keyword")
      Dim isKeyword2 = kind2.ToString.EndsWith("Keyword")

      If isKeyword1 AndAlso isKeyword2 Then Return True
      If isKeyword1 AndAlso kind2 = SyntaxKind.IdentifierToken Then Return True
      If kind1 = SyntaxKind.IdentifierToken AndAlso isKeyword2 Then Return True

      If kind1 = SyntaxKind.IdentifierToken AndAlso
         kind2 = SyntaxKind.IdentifierToken Then
        Return True
      End If

      If kind1 = SyntaxKind.NumberToken AndAlso
         kind2 = SyntaxKind.NumberToken Then
        Return True
      End If

      If kind1 = SyntaxKind.BangToken AndAlso
         kind2 = SyntaxKind.EqualsToken Then
        Return True
      End If

      If kind1 = SyntaxKind.BangToken AndAlso
         kind2 = SyntaxKind.EqualsEqualsToken Then
        Return True
      End If

      If kind1 = SyntaxKind.EqualsToken AndAlso
         kind2 = SyntaxKind.EqualsToken Then
        Return True
      End If

      If kind1 = SyntaxKind.EqualsToken AndAlso
         kind2 = SyntaxKind.EqualsEqualsToken Then
        Return True
      End If

      'TODO: More cases...

      Return False

    End Function

    Private Shared Iterator Function GetTokenPairs() As IEnumerable(Of (kind1 As SyntaxKind, text1 As String, kind2 As SyntaxKind, text2 As String))

      For Each t1 In GetTokens()
        For Each t2 In GetTokens()
          If Not RequiresSeparator(t1.kind, t2.kind) Then
            Yield (t1.kind, t1.text, t2.kind, t2.text)
          End If
        Next
      Next

    End Function

    Private Shared Iterator Function GetTokenPairsWithSeparator() As IEnumerable(Of (kind1 As SyntaxKind, text1 As String,
                                                                                     separatorKind As SyntaxKind, separatorText As String,
                                                                                     kind2 As SyntaxKind, text2 As String))

      For Each t1 In GetTokens()
        For Each t2 In GetTokens()
          If RequiresSeparator(t1.kind, t2.kind) Then
            For Each s In GetSeparators()
              Yield (t1.kind, t1.text,
                     s.kind, s.text,
                     t2.kind, t2.text)
            Next
          End If
        Next
      Next

    End Function

  End Class

End Namespace

