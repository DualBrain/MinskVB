Option Compare Text
Option Explicit On
Option Infer Off
Option Strict On
Imports System
Imports System.CodeDom.Compiler

Namespace Minsk.Generators
    ''' <summary>
    ''' Takes care of opening and closing curly braces for code generation
    ''' </summary>
    Friend Class CurlyIndenter
        Implements IDisposable
        Private _indentedTextWriter As IndentedTextWriter

        ''' <summary>
        ''' Default constructor that maked a tidies creation of the line before the opening curly
        ''' </summary>
        ''' <param name="indentedTextWriter">The writer to use</param>
        ''' <param name="openingLine">any line to write before the curly</param>
        Public Sub New(_indentedTextWriter As IndentedTextWriter, Optional openingLine As String = "")
            _indentedTextWriter = _indentedTextWriter
            If Not String.IsNullOrWhiteSpace(openingLine) Then

                _indentedTextWriter.WriteLine(openingLine)
            End If

            _indentedTextWriter.WriteLine("{")
            _indentedTextWriter.Indent += 1
        End Sub

        ''' <summary>
        ''' When the variable goes out of scope the closing brace is injected and indentation reduced.
        ''' </summary>
        Public Sub Dispose() Implements IDisposable.Dispose
            _indentedTextWriter.Indent -= 1
            _indentedTextWriter.WriteLine("}")
        End Sub
    End Class
End Namespace
