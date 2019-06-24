Option Explicit On
Option Strict On
Option Infer On

Imports System.IO
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis.Binding

  Friend NotInheritable Class ControlFlowGraph

    Private Sub New(start As BasicBlock, [end] As BasicBlock, blocks As List(Of BasicBlock), branches As List(Of BasicBlockBranch))
      Me.Start = start
      Me.End = [end]
      Me.Blocks = blocks
      Me.Branches = branches
    End Sub

    Public ReadOnly Property Start() As BasicBlock
    Public ReadOnly Property [End]() As BasicBlock
    Public ReadOnly Property Blocks() As List(Of BasicBlock)
    Public ReadOnly Property Branches() As List(Of BasicBlockBranch)

    Public NotInheritable Class BasicBlock

      Public Sub New()
      End Sub

      Public Sub New(isStart As Boolean)
        Me.IsStart = isStart
        Me.IsEnd = Not isStart
      End Sub

      Public ReadOnly Property IsStart() As Boolean
      Public ReadOnly Property IsEnd() As Boolean
      Public ReadOnly Property Statements() As New List(Of BoundStatement)()
      Public ReadOnly Property Incoming() As New List(Of BasicBlockBranch)()
      Public ReadOnly Property Outgoing() As New List(Of BasicBlockBranch)()

      Public Overrides Function ToString() As String

        If Me.IsStart Then
          Return "<Start>"
        End If

        If Me.IsEnd Then
          Return "<End>"
        End If

        Using writer = New StringWriter()
          For Each statement In Me.Statements
            statement.WriteTo(writer)
          Next
          Return writer.ToString()
        End Using

      End Function

    End Class

    Public NotInheritable Class BasicBlockBranch

      Public Sub New(from As BasicBlock, [to] As BasicBlock, condition As BoundExpression)
        Me.From = from
        Me.To = [to]
        Me.Condition = condition
      End Sub

      Public ReadOnly Property From() As BasicBlock
      Public ReadOnly Property [To]() As BasicBlock
      Public ReadOnly Property Condition() As BoundExpression

      Public Overrides Function ToString() As String
        If Me.Condition Is Nothing Then
          Return String.Empty
        End If
        Return Me.Condition.ToString()
      End Function

    End Class

    Public NotInheritable Class BasicBlockBuilder

      Private ReadOnly m_statements As New List(Of BoundStatement)()
      Private ReadOnly m_blocks As New List(Of BasicBlock)()

      Public Function Build(block As BoundBlockStatement) As List(Of BasicBlock)
        For Each statement In block.Statements
          Select Case statement.Kind
            Case BoundNodeKind.LabelStatement
              Me.StartBlock()
              Me.m_statements.Add(statement)
            Case BoundNodeKind.GotoStatement, BoundNodeKind.ConditionalGotoStatement, BoundNodeKind.ReturnStatement
              Me.m_statements.Add(statement)
              Me.StartBlock()
            Case BoundNodeKind.VariableDeclaration, BoundNodeKind.ExpressionStatement
              Me.m_statements.Add(statement)
            Case Else
              Throw New Exception($"Unexpected statement: {statement.Kind}")
          End Select
        Next
        Me.EndBlock()
        Return Me.m_blocks.ToList()
      End Function

      Private Sub StartBlock()
        Me.EndBlock()
      End Sub

      Private Sub EndBlock()
        If Me.m_statements.Count > 0 Then
          Dim block = New BasicBlock()
          block.Statements.AddRange(Me.m_statements)
          Me.m_blocks.Add(block)
          Me.m_statements.Clear()
        End If
      End Sub

    End Class

    Public NotInheritable Class GraphBuilder

      Private m_blockFromStatement As New Dictionary(Of BoundStatement, BasicBlock)()
      Private m_blockFromLabel As New Dictionary(Of BoundLabel, BasicBlock)()
      Private m_branches As New List(Of BasicBlockBranch)()
      Private m_start As New BasicBlock(isStart:=True)
      Private m_end As New BasicBlock(isStart:=False)

      Public Function Build(blocks As List(Of BasicBlock)) As ControlFlowGraph
        If Not blocks.Any() Then
          Me.Connect(Me.m_start, Me.m_end)
        Else
          Me.Connect(Me.m_start, blocks.First())
        End If

        For Each block In blocks
          For Each statement In block.Statements
            Me.m_blockFromStatement.Add(statement, block)
            If TypeOf statement Is BoundLabelStatement Then
              Dim labelStatement = CType(statement, BoundLabelStatement)
              Me.m_blockFromLabel.Add(labelStatement.Label, block)
            End If
          Next
        Next

        For i = 0 To blocks.Count - 1
          Dim current = blocks(i)
          Dim [next] = If(i = blocks.Count - 1, Me.m_end, blocks(i + 1))
          For Each statement In current.Statements
            Dim isLastStatementInBlock = statement Is current.Statements.Last()
            Select Case statement.Kind
              Case BoundNodeKind.GotoStatement
                Dim gs = CType(statement, BoundGotoStatement)
                Dim toBlock = Me.m_blockFromLabel(gs.Label)
                Me.Connect(current, toBlock)
              Case BoundNodeKind.ConditionalGotoStatement
                Dim cgs = CType(statement, BoundConditionalGotoStatement)
                Dim thenBlock = Me.m_blockFromLabel(cgs.Label)
                Dim elseBlock = [next]
                Dim negatedCondition = Me.Negate(cgs.Condition)
                Dim thenCondition = If(cgs.JumpIfTrue, cgs.Condition, negatedCondition)
                Dim elseCondition = If(cgs.JumpIfTrue, negatedCondition, cgs.Condition)
                Me.Connect(current, thenBlock, thenCondition)
                Me.Connect(current, elseBlock, elseCondition)
              Case BoundNodeKind.ReturnStatement
                Me.Connect(current, Me.m_end)
              Case BoundNodeKind.VariableDeclaration, BoundNodeKind.LabelStatement, BoundNodeKind.ExpressionStatement
                If isLastStatementInBlock Then
                  Me.Connect(current, [next])
                End If
              Case Else
                Throw New Exception($"Unexpected statement: {statement.Kind}")
            End Select
          Next
        Next

ScanAgain:
        For Each block In blocks
          If Not block.Incoming.Any() Then
            Me.RemoveBlock(blocks, block)
            GoTo ScanAgain
          End If
        Next block

        blocks.Insert(0, Me.m_start)
        blocks.Add(Me.m_end)

        Return New ControlFlowGraph(Me.m_start, Me.m_end, blocks, Me.m_branches)

      End Function

      Private Sub Connect(from As BasicBlock, [to] As BasicBlock, Optional condition As BoundExpression = Nothing)

        If TypeOf condition Is BoundLiteralExpression Then
          Dim l = CType(condition, BoundLiteralExpression)
          Dim value = CBool(l.Value)
          If value Then
            condition = Nothing
          Else
            Return
          End If
        End If

        Dim branch = New BasicBlockBranch(from, [to], condition)
        from.Outgoing.Add(branch)
        [to].Incoming.Add(branch)
        Me.m_branches.Add(branch)

      End Sub

      Private Sub RemoveBlock(blocks As List(Of BasicBlock), block As BasicBlock)

        For Each branch In block.Incoming
          branch.From.Outgoing.Remove(branch)
          Me.m_branches.Remove(branch)
        Next

        For Each branch In block.Outgoing
          branch.To.Incoming.Remove(branch)
          Me.m_branches.Remove(branch)
        Next

        blocks.Remove(block)

      End Sub

      Private Function Negate(condition As BoundExpression) As BoundExpression
        If TypeOf condition Is BoundLiteralExpression Then
          Dim literal = CType(condition, BoundLiteralExpression)
          Dim value = CBool(literal.Value)
          Return New BoundLiteralExpression(Not value)
        End If
        Dim op = BoundUnaryOperator.Bind(SyntaxKind.BangToken, TypeSymbol.Bool)
        Return New BoundUnaryExpression(op, condition)
      End Function
    End Class

    Private Function Quote(text As String) As String
      Return """" & text.Replace("""", "\""") & """"
    End Function

    Public Sub WriteTo(writer As TextWriter)

      writer.WriteLine("digraph G {")

      Dim blockIds = New Dictionary(Of BasicBlock, String)()

      For i = 0 To Me.Blocks.Count - 1
        Dim id = $"N{i}"
        blockIds.Add(Me.Blocks(i), id)
      Next

      For Each block In Me.Blocks
        Dim id = blockIds(block)
        Dim label = Me.Quote(block.ToString().Replace(Environment.NewLine, "\l"))
        writer.WriteLine($"    {id} [label = {label} shape = box]")
      Next

      For Each branch In Me.Branches
        Dim fromId = blockIds(branch.From)
        Dim toId = blockIds(branch.To)
        Dim label = Me.Quote(branch.ToString())
        writer.WriteLine($"    {fromId} -> {toId} [label = {label}]")
      Next

      writer.WriteLine("}")

    End Sub

    Public Shared Function Create(body As BoundBlockStatement) As ControlFlowGraph
      Dim basicBlockBuilder = New BasicBlockBuilder()
      Dim blocks = basicBlockBuilder.Build(body)
      Dim graphBuilder = New GraphBuilder()
      Return graphBuilder.Build(blocks)
    End Function

    Public Shared Function AllPathsReturn(body As BoundBlockStatement) As Boolean

      Dim graph = Create(body)

      For Each branch In graph.End.Incoming
        Dim lastStatement = branch.From.Statements.Last()
        If lastStatement.Kind <> BoundNodeKind.ReturnStatement Then
          Return False
        End If
      Next

      Return True

    End Function

  End Class

End Namespace