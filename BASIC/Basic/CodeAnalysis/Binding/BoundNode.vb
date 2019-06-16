Option Explicit On
Option Strict On
Option Infer On

Imports System.IO
Imports System.Reflection
Imports System.Console
Imports System.ConsoleColor

Namespace Global.Basic.CodeAnalysis.Binding

  Friend MustInherit Class BoundNode

    MustOverride ReadOnly Property Kind As BoundNodeKind

    Public Iterator Function GetChildren() As IEnumerable(Of BoundNode)

      Dim properties = Me.GetType.GetProperties(BindingFlags.[Public] Or BindingFlags.Instance)

      For Each p In properties
        If GetType(BoundNode).IsAssignableFrom(p.PropertyType) Then
          Dim child = DirectCast(p.GetValue(Me), BoundNode)
          If child IsNot Nothing Then
            Yield child
          End If
        ElseIf GetType(IEnumerable(Of BoundNode)).IsAssignableFrom(p.PropertyType) Then
          Dim children = DirectCast(p.GetValue(Me), IEnumerable(Of BoundNode))
          For Each child In children
            If child IsNot Nothing Then
              Yield child
            End If
          Next
        End If
      Next

    End Function

    Private Iterator Function GetProperties() As IEnumerable(Of (name As String, value As Object))

      Dim properties = Me.GetType.GetProperties(BindingFlags.[Public] Or BindingFlags.Instance)

      For Each p In properties

        If p.Name = NameOf(Kind) OrElse
           p.Name = NameOf(BoundBinaryExpression.Op) Then
          Continue For
        End If

        If GetType(BoundNode).IsAssignableFrom(p.PropertyType) OrElse
           GetType(IEnumerable(Of BoundNode)).IsAssignableFrom(p.PropertyType) Then
          Continue For
        End If

        Dim value1 = p.GetValue(Me)
        If value1 IsNot Nothing Then
          Yield (p.Name, value1)
        End If

      Next

    End Function

    Public Sub WriteTo(writer As TextWriter)
      PrettyPrint(writer, Me)
    End Sub

    Private Shared Sub PrettyPrint(writer As TextWriter, node As BoundNode, Optional indent As String = "", Optional isLast As Boolean = True)

      Dim isToConsole = writer Is Console.Out
      Dim marker = If(isLast, "└──", "├──")

      If isToConsole Then
        ForegroundColor = DarkGray
      End If
      writer.Write(indent)
      writer.Write(marker)

      If isToConsole Then
        ForegroundColor = GetColor(node)
      End If

      Dim text = GetText(node)
      writer.Write(text)

      Dim isFirstProperty = True

      For Each p In node.GetProperties
        If isFirstProperty Then
          isFirstProperty = False
        Else
          If isToConsole Then ForegroundColor = DarkGray
          writer.Write(",")
        End If
        writer.Write(" ")
        If isToConsole Then ForegroundColor = Yellow
        writer.Write(p.name)
        If isToConsole Then ForegroundColor = DarkGray
        writer.Write(" = ")
        If isToConsole Then ForegroundColor = DarkYellow
        writer.Write(p.value)
      Next

      If isToConsole Then
        ResetColor()
      End If

      writer.WriteLine()

      indent += If(isLast, "   ", "│  ")

      Dim lastChild = node.GetChildren.LastOrDefault

      For Each child In node.GetChildren
        PrettyPrint(writer, child, indent, child Is lastChild)
      Next

    End Sub

    'Private Shared Sub WriteProperties(writer As TextWriter, node As BoundNode)
    'End Sub

    'Private Shared Sub WriteNode(writer As TextWriter, node As BoundNode)
    '  'TODO: Handle binary and unary expressions
    '  ForegroundColor = GetColor(node)
    '  Dim text = GetText(node)
    '  writer.Write(text)
    '  ResetColor()
    'End Sub

    Private Shared Function GetText(node As BoundNode) As String
      If TypeOf node Is BoundBinaryExpression Then Return DirectCast(node, BoundBinaryExpression).Op.Kind.ToString & "Expression"
      If TypeOf node Is BoundUnaryExpression Then Return DirectCast(node, BoundUnaryExpression).Op.Kind.ToString & "Expression"
      Return node.Kind.ToString
    End Function

    Private Shared Function GetColor(node As BoundNode) As ConsoleColor
      If TypeOf node Is BoundExpression Then Return Blue
      If TypeOf node Is BoundStatement Then Return ConsoleColor.Cyan
      Return Yellow
    End Function

    Public Overrides Function ToString() As String
      Using writer = New StringWriter
        Me.WriteTo(writer)
        Return writer.ToString
      End Using
    End Function

  End Class

End Namespace