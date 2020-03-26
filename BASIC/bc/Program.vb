Imports System
Imports System.Collections.Immutable
Imports System.IO
Imports Basic.CodeAnalysis
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.IO

Module Program

  Sub Main(args As String())

    If args.Length = 0 Then
      Console.WriteLine("usage: bc <source-paths>")
      Return
    End If

    Dim paths = GetFilePaths(args)
    Dim syntaxTrees = New List(Of SyntaxTree)
    Dim hasErrors = False

    For Each path In paths

      If Not IO.File.Exists(path) Then
        Console.WriteLine($"error: file '{path}' doesn't exist.")
        hasErrors = True
        Continue For
      End If

      Dim tree = SyntaxTree.Load(path)

      syntaxTrees.Add(tree)

    Next

    If hasErrors Then
      Return
    End If

    Dim c = New Compilation(syntaxTrees.ToArray)

    Dim result = c.Evaluate(New Dictionary(Of VariableSymbol, Object))

    If Not result.Diagnostics.Any Then
      If result.Value IsNot Nothing Then
        Console.WriteLine(result.Value)
      End If
    Else
      Console.Error.WriteDiagnostics(result.Diagnostics)
    End If

  End Sub

  Private Function GetFilePaths(paths As IEnumerable(Of String)) As IEnumerable(Of String)
    Dim result = New SortedSet(Of String)
    For Each path In paths
      If IO.Directory.Exists(path) Then
        result.UnionWith(Directory.EnumerateFiles(path, "*.ms", SearchOption.AllDirectories))
      Else
        result.Add(path)
      End If
    Next
    Return result
  End Function

End Module
