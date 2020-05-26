Option Explicit On
Option Strict On
Option Infer On

Imports Xunit
Imports Basic.CodeAnalysis.Syntax

Namespace Global.Basic.Tests.CodeAnalysis.Syntax

  Friend NotInheritable Class AssertingEnumerator
    Implements IDisposable

    Private ReadOnly m_enumerator As IEnumerator(Of SyntaxNode)
    Private m_hasErrors As Boolean

    Public Sub New(node As SyntaxNode)
      m_enumerator = Flatten(node).GetEnumerator
    End Sub

    Private Shared Iterator Function Flatten(node As SyntaxNode) As IEnumerable(Of SyntaxNode)

      Dim stack = New Stack(Of SyntaxNode)
      stack.Push(node)

      While stack.Count > 0

        Dim n = stack.Pop
        Yield n

        For Each child In n.GetChildren.Reverse
          stack.Push(child)
        Next

      End While

    End Function

    Public Sub AssertNode(kind As SyntaxKind)
      Try
        Assert.True(m_enumerator.MoveNext())
        Assert.Equal(kind, m_enumerator.Current.Kind)
        Assert.IsNotType(Of SyntaxToken)(m_enumerator.Current)
      Catch When MarkFailed()
        Throw
      End Try
    End Sub

    Public Sub AssertToken(kind As SyntaxKind, text As String)
      Try
        Assert.True(m_enumerator.MoveNext())
        Assert.Equal(kind, m_enumerator.Current.Kind)
        Dim token = Assert.IsType(Of SyntaxToken)(m_enumerator.Current)
        Assert.Equal(text, token.Text)
      Catch When MarkFailed()
        Throw
      End Try
    End Sub

#Region "IDisposable Support"

    Private Function MarkFailed() As Boolean
      m_hasErrors = True
      Return False
    End Function

    Private disposedValue As Boolean ' To detect redundant calls

    ' IDisposable
    Protected Sub Dispose(disposing As Boolean)

      If Not disposedValue Then

        If disposing Then

          ' Dispose managed state (managed objects).

          If Not m_hasErrors Then
            Assert.False(m_enumerator.MoveNext)
          End If

          m_enumerator.Dispose()

        End If

        ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
        ' TODO: set large fields to null.

      End If

      disposedValue = True

    End Sub

    ' TODO: override Finalize() only if Dispose(disposing As Boolean) above has code to free unmanaged resources.
    'Protected Overrides Sub Finalize()
    '    ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
    '    Dispose(False)
    '    MyBase.Finalize()
    'End Sub

    ' This code added by Visual Basic to correctly implement the disposable pattern.
    Public Sub Dispose() Implements IDisposable.Dispose
      ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
      Dispose(True)
      ' TODO: uncomment the following line if Finalize() is overridden above.
      ' GC.SuppressFinalize(Me)
    End Sub

#End Region

  End Class

End Namespace