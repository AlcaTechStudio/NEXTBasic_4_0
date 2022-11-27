Imports System.Text

Module MC68000_Assembly_Module

    Public Function generate_asm(ByVal Ram_pointer As UInt32, ByRef assembly_src As String) As Boolean
        Dim compilacao_OK As Boolean = True

        Dim asm_source_Header As New StringBuilder
        Dim asm_source_Body As New StringBuilder
        Dim asm_source_Botom As New StringBuilder


        ''' TODO '''


        assembly_src = asm_source_Header.ToString() & vbCrLf & asm_source_Body.ToString() & vbCrLf & asm_source_Botom.ToString() & vbCrLf
        Return compilacao_OK
    End Function

End Module
