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
             SyntaxKind.GreaterThanEqualsToken,
             SyntaxKind.GreaterThanToken
          Return 3

        Case SyntaxKind.AmpersandToken,
             SyntaxKind.AmpersandAmpersandToken
          Return 2

        Case SyntaxKind.PipeToken,
             SyntaxKind.PipePipeToken,
             SyntaxKind.HatToken
          Return 1

        Case Else
          Return 0
      End Select

    End Function

    Friend Function GetKeywordKind(text As String) As SyntaxKind

      Select Case text

        Case "true" : Return SyntaxKind.TrueKeyword
        Case "false" : Return SyntaxKind.FalseKeyword

        Case "let" : Return SyntaxKind.LetKeyword
        Case "return" : Return SyntaxKind.ReturnKeyword
        Case "var" : Return SyntaxKind.VarKeyword
        Case "dim" : Return SyntaxKind.DimKeyword

        Case "as" : Return SyntaxKind.AsKeyword
        Case "boolean" : Return SyntaxKind.BooleanKeyword
        Case "short" : Return SyntaxKind.ShortKeyword
        Case "integer" : Return SyntaxKind.IntegerKeyword
        Case "long" : Return SyntaxKind.LongKeyword

        Case "byref" : Return SyntaxKind.ByRefKeyword
        Case "byval" : Return SyntaxKind.ByValKeyword
        Case "optional" : Return SyntaxKind.OptionalKeyword

        ' sub name (value)
        ' end sub
        Case "sub" : Return SyntaxKind.SubKeyword
        ' function name (value)
        '   return result
        ' end function
        Case "function" : Return SyntaxKind.FunctionKeyword

        Case "break" : Return SyntaxKind.BreakKeyword
        'Case "continue": Return SyntaxKind.ContinueKeyword

        ' end
        ' end sub
        ' end function
        ' end if
        ' end while
        Case "end" : Return SyntaxKind.EndKeyword

        ' if true then
        ' elseif false then
        ' else
        ' end if
        Case "if" : Return SyntaxKind.IfKeyword
        Case "then" : Return SyntaxKind.ThenKeyword
        Case "else" : Return SyntaxKind.ElseKeyword
        Case "elseif" : Return SyntaxKind.ElseIfKeyword

        ' exit sub
        ' exit function
        ' continue while
        ' exit while
        ' continue do
        ' exit do
        ' continue for
        ' exit for
        Case "exit" : Return SyntaxKind.ExitKeyword
        Case "continue" : Return SyntaxKind.ContinueKeyword

        ' while
        ' wend
        ' -
        ' while
        ' end while
        Case "while" : Return SyntaxKind.WhileKeyword
        Case "wend" : Return SyntaxKind.WendKeyword
        ' do
        ' loop
        Case "do" : Return SyntaxKind.DoKeyword
        Case "loop" : Return SyntaxKind.LoopKeyword

        ' for x = 1 to 10 step 2
        ' next
        '-
        ' for each value in array
        ' next
        Case "for" : Return SyntaxKind.ForKeyword
        Case "each" : Return SyntaxKind.EachKeyword
        Case "in" : Return SyntaxKind.InKeyword
        Case "to" : Return SyntaxKind.ToKeyword
        Case "step" : Return SyntaxKind.StepKeyword
        Case "next" : Return SyntaxKind.NextKeyword

        Case "print" : Return SyntaxKind.PrintKeyword

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
        Case SyntaxKind.TildeToken : Return "~"
        Case SyntaxKind.LessThanToken : Return "<"
        Case SyntaxKind.LessThanEqualsToken : Return "<="
        Case SyntaxKind.GreaterThanToken : Return ">"
        Case SyntaxKind.GreaterThanEqualsToken : Return ">="
        Case SyntaxKind.AmpersandToken : Return "&"
        Case SyntaxKind.AmpersandAmpersandToken : Return "&&"
        Case SyntaxKind.PipeToken : Return "|"
        Case SyntaxKind.PipePipeToken : Return "||"
        Case SyntaxKind.HatToken : Return "^"
        Case SyntaxKind.EqualsEqualsToken : Return "=="
        Case SyntaxKind.BangEqualsToken : Return "!="
        'Case SyntaxKind.LessThanGreaterThanToken : Return "<>"
        Case SyntaxKind.OpenParenToken : Return "("
        Case SyntaxKind.CloseParenToken : Return ")"
        Case SyntaxKind.OpenBraceToken : Return "{"
        Case SyntaxKind.CloseBraceToken : Return "}"
        Case SyntaxKind.ColonToken : Return ":"
        Case SyntaxKind.CommaToken : Return ","
        Case SyntaxKind.BreakKeyword : Return "break"
        Case SyntaxKind.ContinueKeyword : Return "continue"
        Case SyntaxKind.ElseKeyword : Return "else"
        Case SyntaxKind.FalseKeyword : Return "false"
        Case SyntaxKind.ForKeyword : Return "for"
        Case SyntaxKind.FunctionKeyword : Return "function"
        Case SyntaxKind.IfKeyword : Return "if"
        Case SyntaxKind.LetKeyword : Return "let"
        Case SyntaxKind.ReturnKeyword : Return "return"
        Case SyntaxKind.ToKeyword : Return "to"
        Case SyntaxKind.TrueKeyword : Return "true"
        Case SyntaxKind.VarKeyword : Return "var"
        Case SyntaxKind.WhileKeyword : Return "while"
        Case SyntaxKind.DoKeyword : Return "do"

        Case SyntaxKind.PrintKeyword : Return "print"

          'Case SyntaxKind.NotKeyword : Return "not"
          'Case SyntaxKind.AndKeyword : Return "and"
          'Case SyntaxKind.AndAlsoKeyword : Return "andalso"
          'Case SyntaxKind.OrKeyword : Return "or"
          'Case SyntaxKind.OrElseKeyword : Return "orelse"
          'Case SyntaxKind.DimKeyword : Return "dim"

        Case Else
          Return Nothing
      End Select

    End Function

    <Extension>
    Public Function IsComment(kind As SyntaxKind) As Boolean
      Select Case kind
        Case SyntaxKind.SingleLineCommentTrivia,
             SyntaxKind.MultiLineCommentTrivia
          Return True
        Case Else
          Return False
      End Select
    End Function

    <Extension>
    Public Function IsTrivia(kind As SyntaxKind) As Boolean
      Select Case kind
        Case SyntaxKind.SkippedTextTrivia,
             SyntaxKind.LineBreakTrivia,
             SyntaxKind.WhitespaceTrivia,
             SyntaxKind.SingleLineCommentTrivia,
             SyntaxKind.MultiLineCommentTrivia
          Return True
        Case Else
          Return False
      End Select
    End Function

    <Extension>
    Public Function IsKeyword(kind As SyntaxKind) As Boolean
      Return kind.ToString.EndsWith("Keyword")
    End Function

    <Extension>
    Public Function IsToken(kind As SyntaxKind) As Boolean
      Return Not kind.IsTrivia AndAlso
             (kind.IsKeyword OrElse kind.ToString.EndsWith("Token"))
    End Function

  End Module

End Namespace