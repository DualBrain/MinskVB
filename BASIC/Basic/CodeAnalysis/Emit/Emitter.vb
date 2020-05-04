Option Explicit On
Option Strict On
Option Infer On
Imports System.Collections.Immutable
Imports System.Reflection
Imports Basic.CodeAnalysis.Binding
Imports Ccl = Mono.Cecil
Imports Mono.Cecil.ModuleKind
Imports Mono.Cecil.TypeAttributes
Imports Basic.CodeAnalysis.Symbols
Imports System.Data.Common
Imports Mono.Cecil.Cil
Imports Mono.Cecil

Namespace Global.Basic.CodeAnalysis.Emit

  Friend NotInheritable Class Emitter

    Private ReadOnly _diagnostics As New DiagnosticBag
    Private ReadOnly _knownTypes As New Dictionary(Of TypeSymbol, Ccl.TypeReference)
    Private ReadOnly _consoleWriteLineReference As Ccl.MethodReference
    Private ReadOnly _assemblyDefinition As Ccl.AssemblyDefinition

    Private Sub New(moduleName As String, references() As String)

      Dim assemblies = New List(Of Ccl.AssemblyDefinition)

      For Each reference In references
        Try
          Dim assembly = Ccl.AssemblyDefinition.ReadAssembly(reference)
          assemblies.Add(assembly)
        Catch ex As BadImageFormatException
          _diagnostics.ReportInvalidReference(reference)
        End Try
      Next

      Dim builtInTypes = New List(Of (typeSymbol As TypeSymbol, metadataName As String)) From {
        (TypeSymbol.Any, "System.Object"),
        (TypeSymbol.Bool, "System.Int32"),
        (TypeSymbol.Int, "System.Boolean"),
        (TypeSymbol.String, "System.String"),
        (TypeSymbol.Void, "System.Void")
      }

      Dim assemblyName = New Ccl.AssemblyNameDefinition(moduleName, New Version(1, 0))
      _assemblyDefinition = Ccl.AssemblyDefinition.CreateAssembly(assemblyName, moduleName, Ccl.ModuleKind.Console)
      _knownTypes = New Dictionary(Of TypeSymbol, Ccl.TypeReference)

      For Each entry In builtInTypes
        Dim typeReference = ResolveType(assemblies, entry.typeSymbol.Name, entry.metadataName)
        _knownTypes.Add(entry.typeSymbol, typeReference)
      Next

      _consoleWriteLineReference = ResolveMethod(assemblies, "System.Console", "WriteLine", {"System.String"})

    End Sub

    Public Function GetDiagnostics() As ImmutableArray(Of Diagnostic)
      Return _diagnostics.ToImmutableArray
    End Function

    Public Function Emit(program As BoundProgram, outputPath As String) As ImmutableArray(Of Diagnostic)

      If _diagnostics.Any Then Return _diagnostics.ToImmutableArray

      Dim objectType = _knownTypes(TypeSymbol.Any)
      Dim typeDefinition = New Ccl.TypeDefinition("", "Program", Abstract Or Sealed, objectType)
      _assemblyDefinition.MainModule.Types.Add(typeDefinition)

      Dim voidType = _knownTypes(TypeSymbol.Void)
      Dim mainMethod = New Ccl.MethodDefinition("Main", Ccl.MethodAttributes.Static Or Ccl.MethodAttributes.Private, voidType)
      typeDefinition.Methods.Add(mainMethod)

      Dim ilProcessor = mainMethod.Body.GetILProcessor
      ilProcessor.Emit(OpCodes.Ldstr, "Hello world from Cory!")
      ilProcessor.Emit(OpCodes.Call, _consoleWriteLineReference)
      ilProcessor.Emit(OpCodes.Ret)

      _assemblyDefinition.EntryPoint = mainMethod

      _assemblyDefinition.Write(outputPath)

      Return _diagnostics.ToImmutableArray

    End Function

    Public Shared Function Emit(program As BoundProgram, moduleName As String, references() As String, outputPath As String) As ImmutableArray(Of Diagnostic)

      If program.Diagnostics.Any Then Return program.Diagnostics

      Dim emitter = New Emitter(moduleName, references)
      Return emitter.Emit(program, outputPath)

    End Function

    Private Function ResolveType(assemblies As List(Of AssemblyDefinition),
                                 internalName As String,
                                 metadataName As String) As TypeReference
      Dim foundTypes = assemblies.SelectMany(Function(a) a.Modules).
                                  SelectMany(Function(m) m.Types).
                                  Where(Function(t) t.FullName = metadataName).ToArray
      If foundTypes.Length = 1 Then
        Dim typeReference = _assemblyDefinition.MainModule.ImportReference(foundTypes(0))
        Return typeReference
      ElseIf foundTypes.Length = 0 Then
        _diagnostics.ReportRequiredTypeNotFound(internalName, metadataName)
      Else
        _diagnostics.ReportRequiredTypeAmbiguous(internalName, metadataName, foundTypes)
      End If
      Return Nothing
    End Function

    Private Function ResolveMethod(assemblies As List(Of AssemblyDefinition),
                                   typeName As String,
                                   methodName As String,
                                   parameterTypeNames As String()) As MethodReference
      Dim foundTypes = assemblies.SelectMany(Function(a) a.Modules).
                                  SelectMany(Function(m) m.Types).
                                  Where(Function(t) t.FullName = typeName).ToArray
      If foundTypes.Length = 1 Then
        Dim foundType = foundTypes(0)
        Dim methods = foundType.Methods.Where(Function(m) m.Name = methodName)

        For Each method In methods
          If method.Parameters.Count <> parameterTypeNames.Length Then
            Continue For
          End If
          Dim allParametersMatch = True
          For i = 0 To parameterTypeNames.Length - 1
            If method.Parameters(i).ParameterType.FullName <> parameterTypeNames(i) Then
              allParametersMatch = False
              Exit For
            End If
          Next
          If Not allParametersMatch Then
            Continue For
          End If
          Return _assemblyDefinition.MainModule.ImportReference(method)
        Next
        _diagnostics.ReportRequiredMethodNotFound(typeName, methodName, parameterTypeNames)
        Return Nothing
      ElseIf foundTypes.Length = 0 Then
        _diagnostics.ReportRequiredTypeNotFound(Nothing, typeName)
      Else
        _diagnostics.ReportRequiredTypeAmbiguous(Nothing, typeName, foundTypes)
      End If
      Return Nothing
    End Function

  End Class

End Namespace