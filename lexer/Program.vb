Option Explicit On
Option Strict On
Option Infer On

Imports Basic.CodeAnalysis
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Text
Imports Basic.CodeAnalysis.Authoring
Imports Basic.IO
Imports System.Console
Imports System.ConsoleColor
Imports System.IO

Module Program
  Sub Main(args As String())

    Dim text = <code>
print("hello world")
               </code>.Value.Trim
    Dim source = SourceText.From(text)

    ' The first phase of parsing raw text into 
    ' what will be compiled "simply" processes
    ' the raw text into lines as part of the
    ' resulting SourceText.Lines array.

    Console.WriteLine("=== Source ===")
    Console.WriteLine($"{source}")
    Console.WriteLine("---")
    For Each line In source.Lines
      Console.WriteLine($"=== Line: {line.Span} ===")
      Console.WriteLine($"{line}")
    Next

  End Sub
End Module
