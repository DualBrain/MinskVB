Option Explicit On ' Variables must be "declared".
Option Strict On ' They must have a "type".
Option Infer On ' Where we can, the "type" can be inferred.

' This is a .NET Core 3.0 VB.NET project.
' I added the above Option's to enable how I like
' to write software.

' You can find the code at 
' https://github.com/dualbrain/basic
' Feel free to join in and contribute!

Imports System.Console
Imports bc

' 1 + 2 * 3
'
' Could be...
'
'   +
'  / \ 
' 1   *
'    / \
'   2   3
'
' Desired...
'
'     *
'    / \ 
'   +   3
'  / \
' 1   2


Module Program

  Sub Main(args As String())

    Do

      Write("> ")

      Dim line = ReadLine()

      'TODO: This will eventually go away
      '      once we have some parsing going on.
      If String.IsNullOrWhiteSpace(line) Then
        Exit Do
      End If

      Dim parser = New Parser(line)
      Dim expression = parser.Parse
      Dim color = Console.ForegroundColor
      Console.ForegroundColor = ConsoleColor.DarkGray
      PrettyPrint(expression)
      Console.ResetColor()

    Loop

  End Sub

  Sub PrettyPrint(node As SyntaxNode, Optional indent As String = "", Optional isLast As Boolean = True)

    Dim marker = If(isLast, "└──", "├──")

    Write(indent)
    Write(marker)

    Write($"{node.Kind}")

    If TryCast(node, SyntaxToken)?.Value IsNot Nothing Then
      Write(" ")
      Write(DirectCast(node, SyntaxToken).Value)
    End If

    WriteLine()

    indent += If(isLast, "   ", "│  ")

    Dim lastChild = node.GetChildren.LastOrDefault

    For Each child In node.GetChildren
      PrettyPrint(child, indent, child Is lastChild)
    Next

  End Sub

End Module

Class Parser

  Private ReadOnly Property Tokens As SyntaxToken()
  Private Property Position As Integer

  Sub New(text As String)
    Dim tokens = New List(Of SyntaxToken)
    Dim lexer = New Lexer(text)
    Dim token As SyntaxToken
    Do
      token = lexer.NextToken
      If token.Kind <> SyntaxKind.WhitespaceToken AndAlso
         token.Kind <> SyntaxKind.BadToken Then
        tokens.Add(token)
      End If
    Loop While token.Kind <> SyntaxKind.EndOfFileToken
    Me.Tokens = tokens.ToArray
  End Sub

  Private Function Peek(offset As Integer) As SyntaxToken
    Dim index = Me.Position + offset
    If index >= Me.Tokens.Length Then
      Return Me.Tokens(Me.Tokens.Length - 1)
    End If
    Return Me.Tokens(index)
  End Function

  Private Function Current() As SyntaxToken
    Return Me.Peek(0)
  End Function

  Private Function NextToken() As SyntaxToken
    Dim current = Me.Current
    Me.Position += 1
    Return current
  End Function

  Private Function Match(kind As SyntaxKind) As SyntaxToken
    If Me.Current.Kind = kind Then
      Return Me.NextToken()
    Else
      Return New SyntaxToken(kind, Me.Current.Position, Nothing, Nothing)
    End If
  End Function

  Public Function Parse() As ExpressionSyntax

    Dim left = Me.ParsePrimaryExpression

    While Me.Current.Kind = SyntaxKind.PlusToken OrElse
          Me.Current.Kind = SyntaxKind.MinusToken

      Dim operatorToken = Me.NextToken()
      Dim right = Me.ParsePrimaryExpression()
      left = New BinaryExpressionSyntax(left, operatorToken, right)

    End While

    Return left

  End Function

  Private Function ParsePrimaryExpression() As ExpressionSyntax
    Dim numberToken = Match(SyntaxKind.NumberToken)
    Return New NumberExpressionSyntax(numberToken)
  End Function

End Class

MustInherit Class SyntaxNode

  Public MustOverride ReadOnly Property Kind() As SyntaxKind

  Public MustOverride Function GetChildren() As IEnumerable(Of SyntaxNode)

End Class

MustInherit Class ExpressionSyntax
  Inherits SyntaxNode
End Class

NotInheritable Class NumberExpressionSyntax
  Inherits ExpressionSyntax

  Sub New(numberToken As SyntaxToken)
    Me.NumberToken = numberToken
  End Sub

  Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
    Yield Me.NumberToken
  End Function

  Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.NumberExpression
  Public ReadOnly NumberToken As SyntaxToken

End Class

NotInheritable Class BinaryExpressionSyntax
  Inherits ExpressionSyntax

  Sub New(left As ExpressionSyntax, operatorToken As SyntaxToken, right As ExpressionSyntax)
    Me.Left = left
    Me.OperatorToken = operatorToken
    Me.Right = right
  End Sub

  Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.BinaryExpression
  Public ReadOnly Property Left As ExpressionSyntax
  Public ReadOnly Property OperatorToken As SyntaxToken
  Public ReadOnly Property Right As ExpressionSyntax

  Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
    Yield Me.Left
    Yield Me.OperatorToken
    Yield Me.Right
  End Function
End Class
