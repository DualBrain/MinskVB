Option Compare Text
Option Explicit On
Option Infer Off
Option Strict On

Imports System
Imports System.CodeDom.Compiler
Imports System.Collections.Generic
Imports System.IO
Imports System.Linq
Imports System.Text
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.CSharp.Syntax
Imports Microsoft.CodeAnalysis.Text

Namespace Minsk.Generators
    <Generator>
    Public Class SyntaxNodeGetChildrenGenerator
        Inherits ISourceGenerator
        Public Sub Initialize(context As InitializationContext)
        End Sub

        Public Sub Execute(context As SourceGeneratorContext)
            Dim _sourceText As SourceText

            Dim _compilation = CType(context.Compilation, CSharpCompilation)

            Dim immutableArrayType = _compilation.GetTypeByMetadataName("System.Collections.Immutable.ImmutableArray`1")
            Dim separatedSyntaxListType = _compilation.GetTypeByMetadataName("Minsk.CodeAnalysis.Syntax.SeparatedSyntaxList`1")
            Dim syntaxNodeType = _compilation.GetTypeByMetadataName("Minsk.CodeAnalysis.Syntax.SyntaxNode")

            If immutableArrayType Is Nothing OrElse separatedSyntaxListType Is Nothing OrElse syntaxNodeType Is Nothing Then
                Return
            End If

            Dim types = GetAllTypes(_compilation.Assembly)
            Dim syntaxNodeTypes = types.Where(Function(t) Not t.IsAbstract AndAlso IsPartial(t) AndAlso IsDerivedFrom(t, syntaxNodeType))

            Dim indentString As String = "    "
            Using _stringWriter As System.IO.StringWriter = New StringWriter
                Using _indentedTextWriter As System.CodeDom.Compiler.IndentedTextWriter = New IndentedTextWriter(_stringWriter, indentString)
                    _indentedTextWriter.WriteLine("using System;")
                    _indentedTextWriter.WriteLine("using System.Collections.Generic;")
                    _indentedTextWriter.WriteLine("using System.Collections.Immutable;")
                    _indentedTextWriter.WriteLine()
                    Using nameSpaceCurly = New CurlyIndenter(_indentedTextWriter, "namespace Minsk.CodeAnalysis.Syntax")
                        For Each type In syntaxNodeTypes
                            Using classCurly = New CurlyIndenter(_indentedTextWriter, $"partial class {type.Name}")
                                Using getChildCurly = New CurlyIndenter(_indentedTextWriter, "public override IEnumerable<SyntaxNode> GetChildren()")
                                    For Each [property] In type.GetMembers().OfType(Of IPropertySymbol)()
                                        Dim TempVar As Boolean = TypeOf [property].Type Is INamedTypeSymbol
                                        Dim propertyType As INamedTypeSymbol = [property].Type
                                        If TempVar Then
                                            If IsDerivedFrom(propertyType, syntaxNodeType) Then
                                                Dim canBeNull As Boolean = [property].NullableAnnotation = NullableAnnotation.Annotated
                                                If canBeNull Then
                                                    _indentedTextWriter.WriteLine($"if ({[property].Name} != null)")
                                                    _indentedTextWriter.Indent += 1
                                                End If

                                                _indentedTextWriter.WriteLine($"yield return {[property].Name};")

                                                If canBeNull Then
                                                    _indentedTextWriter.Indent -= 1
                                                End If
                                            ElseIf propertyType.TypeArguments.Length = 1 AndAlso
                                                     IsDerivedFrom(propertyType.TypeArguments(0), syntaxNodeType) AndAlso
                                                     SymbolEqualityComparer.[Default].Equals(propertyType.OriginalDefinition, immutableArrayType) Then
                                                _indentedTextWriter.WriteLine($"foreach (var child in {[property].Name})")
                                                _indentedTextWriter.WriteLine($"{indentString}yield return child;")
                                            ElseIf SymbolEqualityComparer.[Default].Equals(propertyType.OriginalDefinition, separatedSyntaxListType) AndAlso
                                                     IsDerivedFrom(propertyType.TypeArguments(0), syntaxNodeType) Then
                                                _indentedTextWriter.WriteLine($"foreach (var child in {[property].Name}.GetWithSeparators())")
                                                _indentedTextWriter.WriteLine($"{indentString}yield return child;")
                                            End If
                                        End If
                                    Next
                                End Using
                            End Using
                        Next
                    End Using

                    _indentedTextWriter.Flush()
                    _stringWriter.Flush()

                    _sourceText = SourceText.From(_stringWriter.ToString(), Encoding.UTF8)
                End Using
            End Using

            Dim hintName As String = "SyntaxNode_GetChildren.g.cs"
            context.AddSource(hintName, _sourceText)

            ' HACK
            '
            ' Make generator work in VS Code. See src\Directory.Build.props for
            ' details.

            Dim fileName As String = "SyntaxNode_GetChildren.g.cs"
            Dim syntaxNodeFilePath = syntaxNodeType.DeclaringSyntaxReferences.First().SyntaxTree.FilePath
            Dim syntaxDirectory = Path.GetDirectoryName(syntaxNodeFilePath)
            Dim _filePath As String = Path.Combine(syntaxDirectory, fileName)

            If File.Exists(_filePath) Then
                Dim fileText As String = File.ReadAllText(_filePath)
                Dim sourceFileText As Microsoft.CodeAnalysis.Text.SourceText = SourceText.From(fileText, Encoding.UTF8)
                If _sourceText.ContentEquals(sourceFileText) Then
                    Return
                End If
            End If
            Using writer As System.IO.StreamWriter = New StreamWriter(_filePath)
                _sourceText.Write(writer)
            End Using
        End Sub

        Private Function GetAllTypes(symbol As IAssemblySymbol) As IReadOnlyList(Of INamedTypeSymbol)
            Dim result As System.Collections.Generic.List(Of Microsoft.CodeAnalysis.INamedTypeSymbol) = New List(Of INamedTypeSymbol)
            GetAllTypes(result, symbol.GlobalNamespace)
            result.Sort(Function(x, y) x.MetadataName.CompareTo(y.MetadataName))
            Return result
        End Function

        Private Sub GetAllTypes(result As List(Of INamedTypeSymbol), symbol As INamespaceOrTypeSymbol)
            Dim TempVar1 As Boolean = TypeOf symbol Is INamedTypeSymbol
            Dim type As INamedTypeSymbol = symbol
            If TempVar1 Then
                result.Add(type)
            End If

            For Each child As Microsoft.CodeAnalysis.ISymbol In symbol.GetMembers()
                Dim TempVar2 As Boolean = TypeOf child Is INamespaceOrTypeSymbol
                Dim nsChild As INamespaceOrTypeSymbol = child
                If TempVar2 Then
                    GetAllTypes(result, nsChild)
                End If
            Next
        End Sub

        Private Function IsDerivedFrom(type As ITypeSymbol, baseType As INamedTypeSymbol) As Boolean
            Dim current As Microsoft.CodeAnalysis.ITypeSymbol = type

            While current IsNot Nothing

                If SymbolEqualityComparer.[Default].Equals(current, baseType) Then
                    Return True
                End If

                current = current.BaseType
            End While

            Return False
        End Function

        Private Function IsPartial(type As INamedTypeSymbol) As Boolean
            For Each declaration As Microsoft.CodeAnalysis.SyntaxReference In type.DeclaringSyntaxReferences
                Dim syntax As Microsoft.CodeAnalysis.SyntaxNode = declaration.GetSyntax()
                Dim TempVar3 As Boolean = TypeOf syntax Is TypeDeclarationSyntax
                Dim typeDeclaration As TypeDeclarationSyntax = syntax
                If TempVar3 Then
                    For Each modifer In typeDeclaration.Modifiers
                        If modifer.ValueText = "partial" Then
                            Return True
                        End If
                    Next
                End If
            Next

            Return False
        End Function
    End Class
End Namespace