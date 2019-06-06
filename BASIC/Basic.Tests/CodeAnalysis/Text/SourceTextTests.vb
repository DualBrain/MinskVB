Option Explicit On
Option Strict On
Option Infer On

Imports Xunit
Imports Basic.CodeAnalysis.Text

Namespace Global.Basic.Tests.CodeAnalysis.Text

  Public Class SourceTextTests

    Const CR = ChrW(13)
    Const LF = ChrW(10)

    <Theory>
    <InlineData(".", 1)>
    <InlineData("." & CR & LF, 2)>
    <InlineData("." & CR & LF & CR & LF, 3)>
    Public Sub SourceText_IncludesLastLine(text As String, expectedLineCount As Integer)

      Dim source = SourceText.From(text)
      Assert.Equal(expectedLineCount, source.Lines.Length)

    End Sub

  End Class

End Namespace