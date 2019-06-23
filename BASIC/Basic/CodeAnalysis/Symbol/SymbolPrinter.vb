Option Explicit On
Option Strict On
Option Infer On

Imports System.IO
Imports Basic.IO

Namespace Global.Basic.CodeAnalysis.Symbols

  Friend Module SymbolPrinter

    Public Sub WriteTo(symbol As Symbol, writer As TextWriter)
      Select Case symbol.Kind
        Case SymbolKind.Function : WriteFunctionTo(CType(symbol, FunctionSymbol), writer)
        Case SymbolKind.GlobalVariable : WriteGlobalVariableTo(CType(symbol, GlobalVariableSymbol), writer)
        Case SymbolKind.LocalVariable : WriteLocalVariableTo(CType(symbol, LocalVariableSymbol), writer)
        Case SymbolKind.Parameter : WriteParameterTo(CType(symbol, ParameterSymbol), writer)
        Case SymbolKind.Type : WriteTypeTo(CType(symbol, TypeSymbol), writer)
        Case Else
          Throw New Exception($"Unexpected symbol: {symbol.Kind}")
      End Select
    End Sub

    Private Sub WriteFunctionTo(symbol As FunctionSymbol, writer As TextWriter)

      writer.WriteKeyword("function ")
      writer.WriteIdentifier(symbol.Name)
      writer.WritePunctuation("(")

      For i As Integer = 0 To symbol.Parameters.Length - 1
        If i > 0 Then
          writer.WritePunctuation(", ")
        End If
        symbol.Parameters(i).WriteTo(writer)
      Next

      writer.WritePunctuation(")")
      writer.WriteLine()

    End Sub

    Private Sub WriteGlobalVariableTo(symbol As GlobalVariableSymbol, writer As TextWriter)
      writer.WriteKeyword(If(symbol.IsReadOnly, "let ", "var "))
      writer.WriteIdentifier(symbol.Name)
      writer.WritePunctuation(": ")
      symbol.Type.WriteTo(writer)
    End Sub

    Private Sub WriteLocalVariableTo(symbol As LocalVariableSymbol, writer As TextWriter)
      writer.WriteKeyword(If(symbol.IsReadOnly, "let ", "var "))
      writer.WriteIdentifier(symbol.Name)
      writer.WritePunctuation(": ")
      symbol.Type.WriteTo(writer)
    End Sub

    Private Sub WriteParameterTo(symbol As ParameterSymbol, writer As TextWriter)
      writer.WriteIdentifier(symbol.Name)
      writer.WritePunctuation(": ")
      symbol.Type.WriteTo(writer)
    End Sub

    Private Sub WriteTypeTo(symbol As TypeSymbol, writer As TextWriter)
      writer.WriteIdentifier(symbol.Name)
    End Sub

  End Module

End Namespace