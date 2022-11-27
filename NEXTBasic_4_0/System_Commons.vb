Imports System.Drawing
Imports System.Net.Mime.MediaTypeNames
Imports System.Runtime.CompilerServices
Imports System.Text

Module System_Commons

    Public Class Buffer_Entrada_codigo_fonte_S
        Public nome As String
        Public caminho As String
        Public cd_lido As Boolean
        Public codigo As StringBuilder
        Public modificadores As List(Of String)
    End Class

    Public Class Identificadores_NextBasic
        ' Variaveis simples/Matrizes
        Public Variaveis_Globais As New List(Of identificador_S)
        Public Variaveis_Locais As New List(Of identificador_S)
        'Structure
        Public Definicao_Estruturas As New List(Of identificador_S)
        Public Estruturas As New List(Of identificador_S)
        ' Codigo
        Public Funcoes As New List(Of identificador_S)
        Public Subrotinas As New List(Of identificador_S)
        'Labels
        Public Labels_Globais As New List(Of identificador_S)

    End Class



    'TODO -> Terminar essa estrutura
    Public Class identificador_S
        Public id As String
        Public usado As Boolean
        Public endereco As String
        Public valor_inicial As String
        Public tipo_dados As Integer
        Public tamanho_em_bytes As Integer
        'Matrix
        Public N_elementos As UInt32
        ' Funções e Estruturas
        Public parametros_id As List(Of String)
        Public parametros_data_type As List(Of String)
        Public is_pointer As List(Of Boolean)
        Public Valor_Retorno As Integer

        Public Sub New(ByVal id As String, ByVal data_type As Integer, ByVal nelementos As Integer, ByVal inicializador As String, ByVal inicializador_tm_bytes As Integer, ByVal addr As String) 'Variavel x Matriz
            Me.id = id
            Me.tipo_dados = data_type
            Me.N_elementos = nelementos
            Me.tamanho_em_bytes = tipo_de_Dados_em_bytes(data_type) * nelementos
            Me.endereco = If(String.IsNullOrEmpty(addr), id, addr)
            Me.valor_inicial = inicializador
            Me.usado = False
        End Sub
        Public Function get_id() As UInt32
            Return Me.id
        End Function
        Public Function get_tipo_Dados() As UInt32
            Return Me.tipo_dados
        End Function
        Public Function get_tipo_Dados_as_string() As String
            Return tipo_de_Dados_str(Me.tipo_dados)
        End Function
    End Class
    Public Function Pega_linha_source_file(ByVal linha As Int32, ByRef caminho As String) As String

        Dim src_l As String = System.IO.File.ReadAllLines(caminho)(linha)
        Return "Linha ..::" & src_l & " ::.. no arquivo  """ & caminho & ":" & Convert.ToString(linha) & """" & System.Environment.NewLine

    End Function

    Public Function e_indentificador_declarado(ByVal n_ind As String) As Integer
        For Each item As identificador_S In Identificadores_Construcao.Variaveis_Globais
            If String.Compare(item.id, n_ind, True) = 0 Then Return 1
        Next
        For Each item As identificador_S In Identificadores_Construcao.Variaveis_Locais
            If String.Compare(item.id, n_ind, True) = 0 Then Return 2
        Next
        For Each item As identificador_S In Identificadores_Construcao.Funcoes
            If String.Compare(item.id, n_ind, True) = 0 Then Return 3
        Next
        For Each item As identificador_S In Identificadores_Construcao.Subrotinas
            If String.Compare(item.id, n_ind, True) = 0 Then Return 4
        Next
        For Each item As identificador_S In Identificadores_Construcao.Estruturas
            If String.Compare(item.id, n_ind, True) = 0 Then Return 5
        Next
        For Each item As identificador_S In Identificadores_Construcao.Definicao_Estruturas
            If String.Compare(item.id, n_ind, True) = 0 Then Return 6
        Next
        For Each item As identificador_S In Identificadores_Construcao.Labels_Globais
            If String.Compare(item.id, n_ind, True) = 0 Then Return 7
        Next

        Return 0

    End Function

    Public Function declarar_variavel(ByRef id As String, ByVal tipo As Integer, ByVal tamanho_array As Integer, ByVal E_global As Boolean, ByRef inicializador As String, ByVal tam_inicializador As Integer, ByRef addr As String) As String

        If e_indentificador_declarado(id) <> 0 Then
            Return "Erro: Redefinição de Identificador """ & id & """ como variavel " & If(E_global, "Global", "Local") & ", identificador declarado originalmente como " & identificadores_construcao_nome(e_indentificador_declarado(id))
        End If

        If E_global Then
            Identificadores_Construcao.Variaveis_Globais.Add(New identificador_S(id, tipo, tamanho_array, inicializador, tam_inicializador, addr))
            adc_meta(_dec_var, tipo, "_global_" & id, tamanho_array, addr)
        Else
            Identificadores_Construcao.Variaveis_Locais.Add(New identificador_S(id, tipo, tamanho_array, inicializador, tam_inicializador, addr))
            adc_meta(_dec_var, tipo, "_local_" & id, tamanho_array, addr)
        End If

        Return ""
    End Function

    Public Function declarar_label(ByRef id As String) As String
        If e_indentificador_declarado(id) <> 0 Then
            Return "Erro: Redefinição de Identificador """ & id & """ como Label" & ", identificador declarado originalmente como " & identificadores_construcao_nome(e_indentificador_declarado(id))
        End If

        Identificadores_Construcao.Labels_Globais.Add(New identificador_S(id, Data_type_ulong, 1, "", 0, ""))

        Return ""
    End Function


End Module
