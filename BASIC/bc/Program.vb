Imports System
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

    If args.Length > 1 Then
      Console.WriteLine("error: only one path supported right now.")
      Return
    End If

    Dim path = args.Single

    Dim text = IO.File.ReadAllText(path)
    Dim tree = SyntaxTree.Parse(text)
    Dim c = New Compilation(tree)

    Dim result = c.Evaluate(New Dictionary(Of VariableSymbol, Object))

    If Not result.Diagnostics.Any Then
      If result.Value IsNot Nothing Then
        Console.WriteLine(result.Value)
      End If
    Else
      Console.Error.WriteDiagnostics(result.Diagnostics, tree)
    End If

  End Sub

End Module
