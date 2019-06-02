Option Explicit On
Option Strict On
Option Infer On

Imports System.Runtime.CompilerServices

Namespace Global.Basic.CodeAnalysis

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

  End Module

End Namespace