Option Explicit On
Option Strict On
Option Infer On

Imports System.Runtime.CompilerServices

Namespace Global.Basic.CodeAnalysis.Syntax

  Public Module SyntaxFacts

    <Extension()>
    Public Function GetUnaryOperatorPrecedence(kind As SyntaxKind) As Integer

      Select Case kind
        Case SyntaxKind.PlusToken,
             SyntaxKind.MinusToken,
             SyntaxKind.BangToken,
             SyntaxKind.NotKeyword,
             SyntaxKind.TildeToken
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

        Case SyntaxKind.EqualsEqualsToken,
             SyntaxKind.BangEqualsToken,
             SyntaxKind.LessThanToken,
             SyntaxKind.LessThanEqualsToken,
             SyntaxKind.LessThanGreaterThanToken,
             SyntaxKind.GreaterThanEqualsToken,
             SyntaxKind.GreaterThanToken
          Return 3

        Case SyntaxKind.AmpersandToken,
             SyntaxKind.AmpersandAmpersandToken,
             SyntaxKind.AndKeyword
          Return 2

        Case SyntaxKind.PipeToken,
             SyntaxKind.PipePipeToken,
             SyntaxKind.OrKeyword,
             SyntaxKind.HatToken
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

        Case "let"
          Return SyntaxKind.LetKeyword
        Case "var"
          Return SyntaxKind.VarKeyword
        Case "dim"
          Return SyntaxKind.DimKeyword

        Case "function"
          Return SyntaxKind.FunctionKeyword
        Case "if"
          Return SyntaxKind.IfKeyword
        'Case "then"
        '  Return SyntaxKind.ThenKeyword
        Case "else"
          Return SyntaxKind.ElseKeyword
        'Case "elseif"
        '  Return SyntaxKind.ElseIfKeyword
        'Case "endif"
        '  Return SyntaxKind.ElseIfKeyword

        Case "while"
          Return SyntaxKind.WhileKeyword
        Case "do"
          Return SyntaxKind.DoKeyword

        Case "for"
          Return SyntaxKind.ForKeyword
        Case "to"
          Return SyntaxKind.ToKeyword

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
        Case SyntaxKind.AmpersandToken : Return "&"
        Case SyntaxKind.AmpersandAmpersandToken : Return "&&"
        Case SyntaxKind.HatToken : Return "^"
        Case SyntaxKind.TildeToken : Return "~"
        Case SyntaxKind.EqualsEqualsToken : Return "=="
        Case SyntaxKind.BangEqualsToken : Return "!="
        Case SyntaxKind.LessThanToken : Return "<"
        Case SyntaxKind.LessThanEqualsToken : Return "<="
        Case SyntaxKind.LessThanGreaterThanToken : Return "<>"
        Case SyntaxKind.GreaterThanEqualsToken : Return ">="
        Case SyntaxKind.GreaterThanToken : Return ">"
        Case SyntaxKind.PipeToken : Return "|"
        Case SyntaxKind.PipePipeToken : Return "||"
        Case SyntaxKind.OpenParenToken : Return "("
        Case SyntaxKind.CloseParenToken : Return ")"
        Case SyntaxKind.OpenBraceToken : Return "{"
        Case SyntaxKind.CloseBraceToken : Return "}"
        Case SyntaxKind.ColonToken : Return ":"
        Case SyntaxKind.CommaToken : Return ","
        Case SyntaxKind.FalseKeyword : Return "false"
        Case SyntaxKind.TrueKeyword : Return "true"
        Case SyntaxKind.NotKeyword : Return "not"
        Case SyntaxKind.AndKeyword : Return "and"
        Case SyntaxKind.AndAlsoKeyword : Return "andalso"
        Case SyntaxKind.OrKeyword : Return "or"
        Case SyntaxKind.OrElseKeyword : Return "orelse"
        Case SyntaxKind.LetKeyword : Return "let"
        Case SyntaxKind.VarKeyword : Return "var"
        Case SyntaxKind.DimKeyword : Return "dim"

        Case SyntaxKind.FunctionKeyword : Return "function"
        Case SyntaxKind.IfKeyword : Return "if"
        'Case SyntaxKind.ThenKeyword : Return "then"
        Case SyntaxKind.ElseKeyword : Return "else"
          'Case SyntaxKind.ElseIfKeyword : Return "elseif"
          'Case SyntaxKind.EndIfKeyword : Return "endif"

        Case SyntaxKind.WhileKeyword : Return "while"
        Case SyntaxKind.DoKeyword : Return "do"

        Case SyntaxKind.ForKeyword : Return "for"
        Case SyntaxKind.ToKeyword : Return "to"

        Case Else
          Return Nothing
      End Select

    End Function

  End Module

End Namespace