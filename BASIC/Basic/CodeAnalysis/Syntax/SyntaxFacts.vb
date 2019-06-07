﻿Option Explicit On
Option Strict On
Option Infer On

Imports System.Runtime.CompilerServices

Namespace Global.Basic.CodeAnalysis.Syntax

  Public Module SyntaxFacts

    <Extension()>
    Public Function GetUnaryOperatorPrecedence(kind As SyntaxKind) As Integer

      Select Case kind
        Case SyntaxKind.PlusToken, SyntaxKind.MinusToken, SyntaxKind.BangToken, SyntaxKind.NotKeyword
          Return 6

        Case Else
          Return 0
      End Select

    End Function

    <Extension()>
    Public Function GetBinaryOperatorPrecedence(kind As SyntaxKind) As Integer

      Select Case kind

        Case SyntaxKind.StarToken, SyntaxKind.SlashToken
          Return 5
        Case SyntaxKind.PlusToken, SyntaxKind.MinusToken
          Return 4

        Case SyntaxKind.EqualsEqualsToken, SyntaxKind.BangEqualsToken, SyntaxKind.LessThanGreaterThanToken ', SyntaxKind.EqualsToken
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

    Public Iterator Function GetUnaryOperatorKinds() As IEnumerable(Of SyntaxKind)

      Dim kinds = DirectCast([Enum].GetValues(GetType(SyntaxKind)), SyntaxKind())

      For Each kind In kinds
        If GetUnaryOperatorPrecedence(kind) > 0 Then
          Yield kind
        End If
      Next

    End Function

    Public Iterator Function GetBinaryOperatorKinds() As IEnumerable(Of SyntaxKind)

      Dim kinds = DirectCast([Enum].GetValues(GetType(SyntaxKind)), SyntaxKind())

      For Each kind In kinds
        If GetBinaryOperatorPrecedence(kind) > 0 Then
          Yield kind
        End If
      Next

    End Function

    Public Function GetText(kind As SyntaxKind) As String
      Select Case kind
        Case SyntaxKind.PlusToken : Return "+"
        Case SyntaxKind.MinusToken : Return "-"
        Case SyntaxKind.StarToken : Return "*"
        Case SyntaxKind.SlashToken : Return "/"
        Case SyntaxKind.BangToken : Return "!"
        Case SyntaxKind.EqualsToken : Return "="
        Case SyntaxKind.AmpersandAmpersandToken : Return "&&"
        Case SyntaxKind.EqualsEqualsToken : Return "=="
        Case SyntaxKind.BangEqualsToken : Return "!="
        Case SyntaxKind.LessThanGreaterThanToken : Return "<>"
        Case SyntaxKind.PipePipeToken : Return "||"
        Case SyntaxKind.OpenParenToken : Return "("
        Case SyntaxKind.CloseParenToken : Return ")"
        Case SyntaxKind.OpenBraceToken : Return "{"
        Case SyntaxKind.CloseBraceToken : Return "}"
        Case SyntaxKind.FalseKeyword : Return "false"
        Case SyntaxKind.TrueKeyword : Return "true"
        Case SyntaxKind.NotKeyword : Return "not"
        Case SyntaxKind.AndKeyword : Return "and"
        Case SyntaxKind.AndAlsoKeyword : Return "andalso"
        Case SyntaxKind.OrKeyword : Return "or"
        Case SyntaxKind.OrElseKeyword : Return "orelse"
        Case Else
          Return Nothing
      End Select

    End Function

  End Module

End Namespace