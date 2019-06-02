Option Explicit On
Option Strict On
Option Infer On

Imports System.Runtime.CompilerServices

Namespace Global.Basic.CodeAnalysis.Syntax

  Friend Module SyntaxFacts

    <Extension()>
    Public Function GetUnaryOperatorPrecedence(kind As SyntaxKind) As Integer

      Select Case kind
        Case SyntaxKind.PlusToken, SyntaxKind.MinusToken, SyntaxKind.BangToken, SyntaxKind.NotKeyword
          Return 5

        Case Else
          Return 0
      End Select

    End Function

    <Extension()>
    Public Function GetBinaryOperatorPrecedence(kind As SyntaxKind) As Integer

      Select Case kind

        Case SyntaxKind.StarToken, SyntaxKind.SlashToken
          Return 4
        Case SyntaxKind.PlusToken, SyntaxKind.MinusToken
          Return 3

        Case SyntaxKind.AmpersandAmpersandToken, SyntaxKind.AndKeyword
          Return 2

        Case SyntaxKind.PipePipeToken, SyntaxKind.OrKeyword
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

        Case "not"
          Return SyntaxKind.NotKeyword
        Case "and"
          Return SyntaxKind.AndKeyword
        Case "andalso"
          Return SyntaxKind.AndAlsoKeyword
        Case "or"
          Return SyntaxKind.OrKeyword
        Case "orelse"
          Return SyntaxKind.OrElseKeyword

        Case Else
          Return SyntaxKind.IdentifierToken
      End Select

    End Function

  End Module

End Namespace