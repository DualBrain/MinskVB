Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports System.IO
Imports System.Threading

Imports Basic.CodeAnalysis.Binding
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.CodeAnalysis

  Public NotInheritable Class Compilation

    Private m_globalScope As BoundGlobalScope = Nothing

    Public Sub New(ParamArray syntaxTrees() As SyntaxTree)
      Me.New(Nothing, syntaxTrees)
    End Sub

    Private Sub New(previous As Compilation, ParamArray syntaxTrees() As SyntaxTree)
      Me.Previous = previous
      Me.SyntaxTrees = syntaxTrees.ToImmutableArray
    End Sub

    Public ReadOnly Property Previous As Compilation
    Public ReadOnly Property SyntaxTrees As ImmutableArray(Of SyntaxTree)
    Public ReadOnly Property Functions As ImmutableArray(Of FunctionSymbol)
      Get
        Return GlobalScope.Functions
      End Get
    End Property
    Public ReadOnly Property Variables As ImmutableArray(Of VariableSymbol)
      Get
        Return GlobalScope.Variables
      End Get
    End Property

    Friend ReadOnly Property GlobalScope As BoundGlobalScope
      Get
        If m_globalScope Is Nothing Then
          Dim g = Binder.BindGlobalScope(Previous?.GlobalScope, SyntaxTrees)
          Interlocked.CompareExchange(m_globalScope, g, Nothing)
        End If
        Return m_globalScope
      End Get
    End Property

    Public Iterator Function GetSymbols() As IEnumerable(Of Symbol)

      Dim submission = Me
      Dim seeSymbolNames = New HashSet(Of String)

      While submission IsNot Nothing

        For Each f In submission.Functions
          If (seeSymbolNames.Add(f.Name)) Then
            Yield f
          End If
        Next

        For Each v In submission.Variables
          If (seeSymbolNames.Add(v.Name)) Then
            Yield v
          End If
        Next

        submission = submission.Previous

      End While


    End Function

    Public Function ContinueWith(syntax As SyntaxTree) As Compilation
      Return New Compilation(Me, syntax)
    End Function

    Public Function Evaluate(variables As Dictionary(Of VariableSymbol, Object)) As EvaluationResult

      Dim parseDiagnostics = SyntaxTrees.SelectMany(Function(st) st.Diagnostics)

      Dim diagnostics = parseDiagnostics.Concat(GlobalScope.Diagnostics).ToImmutableArray
      If diagnostics.Any Then
        Return New EvaluationResult(diagnostics, Nothing)
      End If

      Dim program = Binder.BindProgram(GlobalScope)

      Dim appPath = Environment.GetCommandLineArgs(0)
      Dim appDirectory = Path.GetDirectoryName(appPath)
      Dim cfgPath = Path.Combine(appDirectory, "cfg.dot")
      Dim cfgStatement = If(Not program.Statement.Statements.Any AndAlso program.Functions.Any, program.Functions.Last.Value, program.Statement)
      Dim cfg = ControlFlowGraph.Create(cfgStatement)
      Using streamWriter As New StreamWriter(cfgPath)
        cfg.WriteTo(streamWriter)
      End Using

      If program.Diagnostics.Any Then
        Return New EvaluationResult(program.Diagnostics.ToImmutableArray, Nothing)
      End If

      Dim evaluator = New Evaluator(program, variables)
      Dim value = evaluator.Evaluate

      Return New EvaluationResult(ImmutableArray(Of Diagnostic).Empty, value)

    End Function

    Public Sub EmitTree(writer As TextWriter)
      Dim program = Binder.BindProgram(GlobalScope)
      If program.Statement.Statements.Any() Then
        program.Statement.WriteTo(writer)
      Else
        For Each functionBody In program.Functions
          If Not GlobalScope.Functions.Contains(functionBody.Key) Then
            Continue For
          End If
          functionBody.Key.WriteTo(writer)
          writer.WriteLine()
          functionBody.Value.WriteTo(writer)
        Next
      End If
    End Sub

    Public Sub EmitTree(symbol As FunctionSymbol, writer As TextWriter)

      Dim program = Binder.BindProgram(GlobalScope)
      Dim body As BoundBlockStatement
      If Not program.Functions.TryGetValue(symbol, body) Then
        Return
      End If

      symbol.WriteTo(writer)
      writer.WriteLine()
      body.WriteTo(writer)

    End Sub

  End Class

End Namespace