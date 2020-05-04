Option Explicit On
Option Strict On
Option Infer On
Imports System.Collections.Immutable
Imports System.Reflection
Imports Basic.CodeAnalysis.Binding
Imports Ccl = Mono.Cecil
Imports Basic.CodeAnalysis.Symbols
Imports Mono.Cecil.Cil
Imports Mono.Cecil
Imports Mono.Cecil.Rocks

Namespace Global.Basic.CodeAnalysis.Emit

  Friend NotInheritable Class Emitter

    Private ReadOnly _diagnostics As New DiagnosticBag
    Private ReadOnly _knownTypes As New Dictionary(Of TypeSymbol, Ccl.TypeReference)
    Private ReadOnly _consoleWriteLineReference As Ccl.MethodReference
    Private ReadOnly _assemblyDefinition As Ccl.AssemblyDefinition
    Private ReadOnly _methods As New Dictionary(Of FunctionSymbol, MethodDefinition)
    Private _typeDefinition As Ccl.TypeDefinition

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
        Dim typeReference = Emit_ResolveType(assemblies, entry.typeSymbol.Name, entry.metadataName)
        _knownTypes.Add(entry.typeSymbol, typeReference)
      Next

      _consoleWriteLineReference = Emit_ResolveMethod(assemblies, "System.Console", "WriteLine", {"System.String"})

    End Sub

    Public Shared Function Emit(program As BoundProgram, moduleName As String, references() As String, outputPath As String) As ImmutableArray(Of Diagnostic)

      If program.Diagnostics.Any Then Return program.Diagnostics

      Dim emitter = New Emitter(moduleName, references)
      Return emitter.Emit(program, outputPath)

    End Function

    Public Function Emit(program As BoundProgram, outputPath As String) As ImmutableArray(Of Diagnostic)

      If _diagnostics.Any Then Return _diagnostics.ToImmutableArray

      Dim objectType = _knownTypes(TypeSymbol.Any)
      _typeDefinition = New Ccl.TypeDefinition("", "Program", Ccl.TypeAttributes.Abstract Or Ccl.TypeAttributes.Sealed, objectType)
      _assemblyDefinition.MainModule.Types.Add(_typeDefinition)

      For Each functionWithBody In program.Functions
        EmitFunctionDeclaration(functionWithBody.Key)
      Next

      For Each functionWithBody In program.Functions
        EmitFunctionBody(functionWithBody.Key, functionWithBody.Value)
      Next

      If program.MainFunction IsNot Nothing Then
        _assemblyDefinition.EntryPoint = _methods(program.MainFunction)
      End If

      _assemblyDefinition.Write(outputPath)

      Return _diagnostics.ToImmutableArray

    End Function

    Private Sub EmitFunctionDeclaration(func As FunctionSymbol)
      Dim voidType = _knownTypes(TypeSymbol.Void)
      Dim method = New MethodDefinition(func.Name, Ccl.MethodAttributes.Static Or Ccl.MethodAttributes.Private, voidType)
      _typeDefinition.Methods.Add(method)
      _methods.Add(func, method)
    End Sub

    Private Sub EmitFunctionBody(func As FunctionSymbol, body As BoundBlockStatement)
      Dim method = _methods(func)
      Dim ilProcessor = method.Body.GetILProcessor
      For Each statement In body.Statements
        EmitStatement(ilProcessor, statement)
      Next
      'HACK: We should make sure that our bound tree has explicit returns.
      If func.Type Is TypeSymbol.Void Then ilProcessor.Emit(OpCodes.Ret)
      method.Body.OptimizeMacros
    End Sub

    Private Sub EmitStatement(ilProcessor As ILProcessor, node As BoundStatement)
      Select Case node.Kind
        Case BoundNodeKind.VariableDeclaration : EmitVariableDeclaration(ilProcessor, CType(node, BoundVariableDeclaration))
        Case BoundNodeKind.LabelStatement : EmitLabelStatement(ilProcessor, CType(node, BoundLabelStatement))
        Case BoundNodeKind.GotoStatement : EmitGotoStatement(ilProcessor, CType(node, BoundGotoStatement))
        Case BoundNodeKind.ConditionalGotoStatement : EmitConditionalGotoStatement(ilProcessor, CType(node, BoundConditionalGotoStatement))
        Case BoundNodeKind.ReturnStatement : EmitReturnStatement(ilProcessor, CType(node, BoundReturnStatement))
        Case BoundNodeKind.ExpressionStatement : EmitExpressionStatement(ilProcessor, CType(node, BoundExpressionStatement))
        Case Else
          Throw New Exception($"Unexpected node kind {node.Kind}")
      End Select
    End Sub

    Private Sub EmitVariableDeclaration(ilProcessor As ILProcessor, node As BoundVariableDeclaration)
      Throw New NotImplementedException()
    End Sub

    Private Sub EmitLabelStatement(ilProcessor As ILProcessor, node As BoundLabelStatement)
      Throw New NotImplementedException()
    End Sub

    Private Sub EmitGotoStatement(ilProcessor As ILProcessor, node As BoundGotoStatement)
      Throw New NotImplementedException()
    End Sub

    Private Sub EmitConditionalGotoStatement(ilProcessor As ILProcessor, node As BoundConditionalGotoStatement)
      Throw New NotImplementedException()
    End Sub

    Private Sub EmitReturnStatement(ilProcessor As ILProcessor, node As BoundReturnStatement)
      Throw New NotImplementedException()
    End Sub

    Private Sub EmitExpressionStatement(ilProcessor As ILProcessor, node As BoundExpressionStatement)
      EmitExpression(ilProcessor, node.Expression)
      If node.Expression.Type IsNot TypeSymbol.Void Then
        ilProcessor.Emit(OpCodes.Pop)
      End If
    End Sub

    Private Sub EmitExpression(ilProcessor As ILProcessor, node As BoundExpression)
      Select Case node.Kind
        Case BoundNodeKind.LiteralExpression : EmitLiteralExpression(ilProcessor, CType(node, BoundLiteralExpression))
        Case BoundNodeKind.VariableExpression : EmitVariableExpression(ilProcessor, CType(node, BoundVariableExpression))
        Case BoundNodeKind.AssignmentExpression : EmitAssignmentExpression(ilProcessor, CType(node, BoundAssignmentExpression))
        Case BoundNodeKind.UnaryExpression : EmitUnaryExpression(ilProcessor, CType(node, BoundUnaryExpression))
        Case BoundNodeKind.BinaryExpression : EmitBinaryExpression(ilProcessor, CType(node, BoundBinaryExpression))
        Case BoundNodeKind.CallExpression : EmitCallExpression(ilProcessor, CType(node, BoundCallExpression))
        Case BoundNodeKind.ConversionExpression : EmitConversionExpression(ilProcessor, CType(node, BoundConversionExpression))
        Case Else
          Throw New Exception($"Unexpected node kind {node.Kind}")
      End Select
    End Sub

    Private Sub EmitLiteralExpression(ilProcessor As ILProcessor, node As BoundLiteralExpression)
      If node.Type Is TypeSymbol.Bool Then
        Dim value = CBool(node.Value)
        Dim instruction = If(value, OpCodes.Ldc_I4_1, OpCodes.Ldc_I4_0)
        ilProcessor.Emit(instruction)
      ElseIf node.Type Is TypeSymbol.Int Then
        Dim value = CInt(node.Value)
        ilProcessor.Emit(OpCodes.Ldc_I4, value)
      ElseIf node.Type Is TypeSymbol.String Then
        Dim value = CStr(node.Value)
        ilProcessor.Emit(OpCodes.Ldstr, value)
      Else
        Throw New Exception($"Unexpected literal type: {node.Type}")
      End If
    End Sub

    Private Sub EmitVariableExpression(ilProcessor As ILProcessor, node As BoundVariableExpression)
      Throw New NotImplementedException()
    End Sub

    Private Sub EmitAssignmentExpression(ilProcessor As ILProcessor, node As BoundAssignmentExpression)
      Throw New NotImplementedException()
    End Sub

    Private Sub EmitUnaryExpression(ilProcessor As ILProcessor, node As BoundUnaryExpression)
      Throw New NotImplementedException()
    End Sub

    Private Sub EmitBinaryExpression(ilProcessor As ILProcessor, node As BoundBinaryExpression)
      Throw New NotImplementedException()
    End Sub

    Private Sub EmitCallExpression(ilProcessor As ILProcessor, node As BoundCallExpression)

      For Each argument In node.Arguments
        EmitExpression(ilProcessor, argument)
      Next

      If node.Function Is Print Then
        ilProcessor.Emit(OpCodes.Call, _consoleWriteLineReference)
      ElseIf node.Function Is Input Then
        Throw New NotImplementedException
      ElseIf node.Function Is Rnd Then
        Throw New NotImplementedException
      Else
        Dim methodDefinition = _methods(node.Function)
        ilProcessor.Emit(OpCodes.Call, methodDefinition)
      End If

    End Sub

    Private Sub EmitConversionExpression(ilProcessor As ILProcessor, node As BoundConversionExpression)
      Throw New NotImplementedException()
    End Sub

#Region "Converted from 'inline' functions."

    Private Function Emit_ResolveType(assemblies As List(Of AssemblyDefinition),
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

    Private Function Emit_ResolveMethod(assemblies As List(Of AssemblyDefinition),
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

#End Region

  End Class

End Namespace