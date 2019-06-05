Option Explicit On
Option Strict On
Option Infer On

Imports Xunit
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.Tests.CodeAnalysis.Syntax

  Public Class SyntaxFactTests

    <Theory>
    <MemberData(NameOf(GetSyntaxKindData))>
    Public Sub SyntaxFact_GetText_RoundTrips(kind As SyntaxKind)
      Dim text = SyntaxFacts.GetText(kind)
      If text Is Nothing Then
        Return
      End If
      Dim tokens = SyntaxTree.ParseTokens(text)
      Dim token = Assert.Single(tokens)
      Assert.Equal(kind, token.Kind)
      Assert.Equal(text, token.Text)
    End Sub

    Public Shared Iterator Function GetSyntaxKindData() As IEnumerable(Of Object())

      Dim kinds = DirectCast([Enum].GetValues(GetType(SyntaxKind)), SyntaxKind())
      For Each kind In kinds
        Yield New Object() {kind}
      Next

    End Function

  End Class

End Namespace