Option Explicit On
Option Strict On
Option Infer On

Imports System.Runtime.CompilerServices

Namespace Global.Basic.CodeAnalysis.Syntax

  Friend Module SyntaxFacts

    <Extension()>
    Public Function GetUnaryOperatorPrecedence(kind As SyntaxKind) As Integer

      Select Case kind
        Case SyntaxKind.PlusToken, SyntaxKind.MinusToken
          Return 3
        Case Else
          Return 0
      End Select

    End Function

    <Extension()>
    Public Function GetBinaryOperatorPrecedence(kind As SyntaxKind) As Integer

      Select Case kind
        Case SyntaxKind.StarToken, SyntaxKind.SlashToken
          Return 2
        Case SyntaxKind.PlusToken, SyntaxKind.MinusToken
          Return 1
        Case Else
          Return 0
      End Select

    End Function

    Friend Function GetKeywordKind(text As String) As SyntaxKind
      Select Case text.ToLower
        Case "true"
          Return SyntaxKind.TrueKeyword
        Case "false"
          Return SyntaxKind.FalseKeyword
        Case Else
          Return SyntaxKind.IdentifierToken
      End Select

    End Function
  End Module

End Namespace