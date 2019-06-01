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

    Dim showTree = False

    Do

      Write("> ")

      Dim line = ReadLine()

      ' Handle "special commands / circumstances" within the "REPL".

      If String.IsNullOrWhiteSpace(line) Then
        Continue Do
      End If

      Select Case line.ToLower
        Case "option tree on"
          showTree = True : Continue Do
        Case "option tree off"
          showTree = False : Continue Do
        Case "cls"
          Clear() : Continue Do
        Case "exit"
          Exit Do
        Case Else
      End Select

      ' Otherwise, attempt to parse what was entered...

      Dim tree = SyntaxTree.Parse(line)

      ' Only show the parse tree if we have enabled doing so.
      If showTree Then
        Dim color = Console.ForegroundColor
        Console.ForegroundColor = ConsoleColor.DarkGray
        PrettyPrint(tree.Root)
        Console.ResetColor()
      End If

      If Not tree.Diagnostics.Any Then
        ' No errors detected, attemp to evaluate (execute).
        Dim e = New Evaluator(tree.Root)
        Dim result = e.Evaluate
        WriteLine(result)
      Else
        ' We have errors, so don't try to evaluate (execute).
        Console.ForegroundColor = ConsoleColor.DarkRed
        For Each diagnostic In tree.Diagnostics
          WriteLine(diagnostic)
        Next
        Console.ResetColor()
      End If

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

NotInheritable Class SyntaxTree

  Sub New(diagnostics As IEnumerable(Of String), root As ExpressionSyntax, endOfFileToken As SyntaxToken)
    Me.Diagnostics = diagnostics.ToArray
    Me.Root = root
    Me.EndOfFileToken = endOfFileToken
  End Sub

  Public ReadOnly Property Diagnostics As IReadOnlyList(Of String)
  Public ReadOnly Property Root As ExpressionSyntax
  Public ReadOnly Property EndOfFileToken As SyntaxToken

  Public Shared Function Parse(text As String) As SyntaxTree
    Dim parser = New Parser(text)
    Return parser.Parse
  End Function


End Class

Class Parser

  Private ReadOnly Property Tokens As SyntaxToken()
  Private Property Position As Integer

  Private m_diagnostics As List(Of String) = New List(Of String)

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
    Me.m_diagnostics.AddRange(lexer.Diagnostics)
  End Sub

  Public ReadOnly Property Diagnostics As IEnumerable(Of String)
    Get
      Return Me.m_diagnostics
    End Get
  End Property

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
      Me.m_diagnostics.Add($"ERROR: Unexpected token <{Me.Current.Kind}>, expected <{kind}>")
      Return New SyntaxToken(kind, Me.Current.Position, Nothing, Nothing)
    End If
  End Function

  Private Function ParseExpression() As ExpressionSyntax
    Return Me.ParseTerm()
  End Function

  Public Function Parse() As SyntaxTree
    Dim expression = Me.ParseTerm
    Dim endOfFileToken = Me.Match(SyntaxKind.EndOfFileToken)
    Return New SyntaxTree(Me.m_diagnostics, expression, endOfFileToken)
  End Function

  Private Function ParseTerm() As ExpressionSyntax

    Debug.WriteLine("ParseTerm")

    Dim left = Me.ParseFactor

    While Me.Current.Kind = SyntaxKind.PlusToken OrElse
          Me.Current.Kind = SyntaxKind.MinusToken

      Dim operatorToken = Me.NextToken()
      Dim right = Me.ParsePrimaryExpression()
      left = New BinaryExpressionSyntax(left, operatorToken, right)

    End While

    Return left

  End Function

  Private Function ParseFactor() As ExpressionSyntax

    Debug.WriteLine("ParseFactor")

    Dim left = Me.ParsePrimaryExpression

    While Me.Current.Kind = SyntaxKind.StarToken OrElse
          Me.Current.Kind = SyntaxKind.SlashToken

      Dim operatorToken = Me.NextToken()
      Dim right = Me.ParsePrimaryExpression()
      left = New BinaryExpressionSyntax(left, operatorToken, right)

    End While

    Return left

  End Function
  Private Function ParsePrimaryExpression() As ExpressionSyntax

    If Me.Current.Kind = SyntaxKind.OpenParenToken Then
      Dim left = Me.NextToken
      Dim expression = Me.ParseExpression
      Dim right = Me.Match(SyntaxKind.CloseParenToken)
      Return New ParenExpressionSyntax(left, expression, right)
    End If

    Dim numberToken = Match(SyntaxKind.NumberToken)
    Return New NumberExpressionSyntax(numberToken)
  End Function

End Class

MustInherit Class SyntaxNode

  Public MustOverride ReadOnly Property Kind() As SyntaxKind

  Public MustOverride Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)

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

NotInheritable Class ParenExpressionSyntax
  Inherits ExpressionSyntax

  Sub New(openParenToken As SyntaxToken, expression As ExpressionSyntax, closeParenToken As SyntaxToken)
    Me.OpenParenToken = openParenToken
    Me.Expression = expression
    Me.CloseParenToken = closeParenToken
  End Sub

  Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ParenExpression
  Public ReadOnly Property OpenParenToken As SyntaxToken
  Public ReadOnly Property Expression As ExpressionSyntax
  Public ReadOnly Property CloseParenToken As SyntaxToken

  Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
    Yield Me.OpenParenToken
    Yield Me.Expression
    Yield Me.CloseParenToken
  End Function

End Class

Class Evaluator

  Sub New(root As ExpressionSyntax)
    Me.Root = root
  End Sub

  Public ReadOnly Property Root As ExpressionSyntax

  Public Function Evaluate() As Integer
    Return Me.EvaluateExpression(Me.Root)
  End Function

  Private Function EvaluateExpression(node As ExpressionSyntax) As Integer
    ' BinaryExpression
    ' Number Expression

    If TypeOf node Is NumberExpressionSyntax Then
      Return CInt(DirectCast(node, NumberExpressionSyntax).NumberToken.Value)
    End If

    If TypeOf node Is BinaryExpressionSyntax Then
      Dim b = DirectCast(node, BinaryExpressionSyntax)
      Dim left = Me.EvaluateExpression(b.Left)
      Dim right = Me.EvaluateExpression(b.Right)
      Select Case b.OperatorToken.Kind
        Case SyntaxKind.PlusToken : Return left + right
        Case SyntaxKind.MinusToken : Return left - right
        Case SyntaxKind.StarToken : Return left * right
        Case SyntaxKind.SlashToken : Return left \ right
        Case Else
          Throw New Exception($"Unexpected binary operator {b.OperatorToken.Kind}")
      End Select
    End If

    If TypeOf node Is ParenExpressionSyntax Then
      Dim p = DirectCast(node, ParenExpressionSyntax)
      Return Me.EvaluateExpression(p.Expression)
    End If

    Throw New Exception($"Unexpected node {node.Kind}")

  End Function
End Class