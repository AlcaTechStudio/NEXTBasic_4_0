Imports System
Imports System.IO
Imports System.Text
Imports System.Text.RegularExpressions
' nextbasic.exe -rFF0000 -sFFFFFF -v -a "Endereco source"
Module Program

    Public Identificadores As New List(Of identificador_S)
    Public Identificadores_locais As New List(Of identificador_S)
    Public pasta_projeto As String
    Dim codigo_fonte_lst As New List(Of Buffer_Entrada_codigo_fonte_S)
    Dim codigo_corrente As StringBuilder


    Structure Flags_Compilador
        Dim Exportar_Debug_Info As Boolean
        Dim Exportar_ASM As Boolean
        Dim Declaracao_Auto_variaveis As Boolean
        Dim Gerar_Vector_table As Boolean
        Dim Ponteiro_Inicio_Heap As UInt32
        Dim ponteiro_Inicio_Stack As UInt32

    End Structure
    Function Main(args As String()) As Integer


        Dim nome_projeto As String = ""

        Dim r_param As New RegularExpressions.Regex("-r[0-9a-fA-F]+", RegexOptions.IgnorePatternWhitespace)
        Dim s_param As New RegularExpressions.Regex("-s[0-9a-fA-F]+", RegexOptions.IgnorePatternWhitespace)

        Dim Comp_Flags As New Flags_Compilador


        Directory.SetCurrentDirectory(System.AppDomain.CurrentDomain.BaseDirectory)

        Console.WriteLine("NEXTBasic Compiler MC68000 v4.0")
        Console.WriteLine("")

        For i = 0 To (args.Length - 1)

            Select Case True
                Case (String.Compare("-?", args(i), True) = 0)
                    Console.WriteLine(" Alca_Tech NEXTBasic MC68000 compiler 4.0 Build 16072022")
                    Console.WriteLine("  -r Define o endereco de inicio da memoria RAM")
                    Console.WriteLine("  -s Define o endereco de inicio do Stack Pointer")
                    Console.WriteLine("  -v O compilador Gera a vector Table ")
                    Console.WriteLine("  -a Declaracao Automatica de Variavel, qualquer indentificador nao previamente declarado sera declarado automaticamente como integer")
                    Console.WriteLine("  -oa O compilador exporta a meta data resultante da compilacao para a pasta debug ")
                    Console.WriteLine("  -d O compilador exporta os arquivos  lst e informacoes de Debug")
                    End
                Case (String.Compare("-oa", args(i), True) = 0)
                    Comp_Flags.Exportar_ASM = True
                Case r_param.IsMatch(args(i))
                    Try
                        Comp_Flags.Ponteiro_Inicio_Heap = Convert.ToUInt32(args(i).Substring(2), 16)
                        If Comp_Flags.Ponteiro_Inicio_Heap Mod 2 Then Comp_Flags.Ponteiro_Inicio_Heap -= 1
                        Console.WriteLine("Endereco de inicio da Memoria RAM: 0x" & args(i).Substring(2))
                    Catch ex As Exception
                        Console.WriteLine("Nao foi possivel identificar o parametro no comando -r")
                        'ok = False
                    End Try
                Case s_param.IsMatch(args(i))
                    Try
                        Comp_Flags.ponteiro_Inicio_Stack = Convert.ToUInt32(args(i).Substring(2), 16)
                        If Comp_Flags.ponteiro_Inicio_Stack Mod 2 Then Comp_Flags.ponteiro_Inicio_Stack -= 1
                        Console.WriteLine("Endereco de inicio do Stack Pointer: 0x" & args(i).Substring(2))
                    Catch ex As Exception
                        Console.WriteLine("Nao foi possivel identificar o parametro no comando -s")
                    End Try
                Case args(i).Contains("-a") ' Auto declarar variaveis
                    Comp_Flags.Declaracao_Auto_variaveis = True
                    Console.WriteLine("Flag Auto declaracao de variaveis = true")
                Case args(i).Contains("-v") ' Gerar Vector Table
                    Comp_Flags.Gerar_Vector_table = True
                    Console.WriteLine("Flag gerar vector Table = true")
                Case args(i).Contains("-d") ' Exportar Debug Info
                    Comp_Flags.Exportar_Debug_Info = True
                    Console.WriteLine("Flag Exportar arquivo para Debug = true")
                Case System.IO.File.Exists(args(i)) ' e o source path                  
                    Adicionar_a_lista_compilacao(args(i))
                    pasta_projeto = System.IO.Path.GetDirectoryName(Path.GetFullPath(args(i)))
                    nome_projeto = Path.GetFileNameWithoutExtension(args(i))
                    Console.WriteLine("Projeto a ser construido: " & nome_projeto)
                Case Else
                    Console.WriteLine("Parametro nao reconhecido: " & args(i))
                    Return 1
            End Select
        Next

        Setup()
        Dim indice_codigos_a_compilar As Integer = 0
        Dim cronometro As Stopwatch = Stopwatch.StartNew() ' Timer que mede o tempo de execucao do código
        ' ''''''''''''''''''''''' Pre-Processor -> Analise '''''''''''''''''''''''''''''''''''''
        'While (codigo_fonte_lst.Count <> indice_codigos_a_compilar)
        For indice_codigos_a_compilar = 0 To codigo_fonte_lst.Count - 1
            If codigo_fonte_lst(indice_codigos_a_compilar).cd_lido = False Then
                Console.WriteLine("Lendo Arquivo de Codigo fonte: " & codigo_fonte_lst(indice_codigos_a_compilar).caminho & System.Environment.NewLine & System.Environment.NewLine)
                Try
                    ' Lendo Arquivo de Código Fonte
                    codigo_fonte_lst(indice_codigos_a_compilar).codigo = New StringBuilder(System.IO.File.ReadAllText(codigo_fonte_lst(indice_codigos_a_compilar).caminho))
                    ' Sanitizando Arquivo de código fonte
                    Console.WriteLine("Sanitizando: " & codigo_fonte_lst(indice_codigos_a_compilar).nome & System.Environment.NewLine & System.Environment.NewLine)
                    sanitizador_Gold_Parser(codigo_fonte_lst(indice_codigos_a_compilar).codigo)
                    'Executando Pre-Processor 
                    Console.WriteLine("Executando Escaneamento do Pre-Processor: " & codigo_fonte_lst(indice_codigos_a_compilar).nome & System.Environment.NewLine & System.Environment.NewLine)
                    If Preprocessor_Scan(codigo_fonte_lst(indice_codigos_a_compilar)) = False Then
                        'Pre-Processor Rejeitou o código
                        cronometro.Stop()
                        Console.WriteLine("Erro: Pre-Processor rejeitou o Arquivo: """ & codigo_fonte_lst(indice_codigos_a_compilar).caminho & """")
                        Return 1
                        Exit Try
                    End If
                Catch ex As Exception
                    cronometro.Stop()
                    Console.WriteLine(ex.ToString())
                    Console.WriteLine("Erro ao Abrir o Arquivo de codigo fonte """ & codigo_fonte_lst(indice_codigos_a_compilar).caminho & """")
                    Console.WriteLine("A compilacao nao pode prosseguir kraio...")
                    Return 1
                End Try
            End If
            indice_codigos_a_compilar += 1
        Next
        'End While
        ' ''''''''''''''''''''''' Pre-Processor -> Modificação '''''''''''''''''''''''''''''''''''''
        For Each codigo_a_aplicar As Buffer_Entrada_codigo_fonte_S In codigo_fonte_lst
            preprocessor_Apply(codigo_a_aplicar)
        Next
        ' ''''''''''''''''''''''' Compilação Basic p/ Three Code Address '''''''''''''''''''''''''''''''''''''
        For Each codigo_a_compilar As Buffer_Entrada_codigo_fonte_S In codigo_fonte_lst
            If compile_to_three_code_address(codigo_a_compilar) = False Then
                cronometro.Stop()
                Console.WriteLine("Erro: O Compilador rejeitou o Arquivo: """ & codigo_a_compilar.caminho & """")
                Return 1
            End If
        Next
        ' ''''''''''''''''''''''' Compilação Three Code Address p/ Assembly MC68000 '''''''''''''''''''''''''''''''''''''
        Dim assembly_source_code As String = ""

        If generate_asm(Comp_Flags.Ponteiro_Inicio_Heap, assembly_source_code) = False Then
            cronometro.Stop()
            Console.WriteLine("Erro: Na geracao do Arquivo Assembly")
            Return 1
        End If

        Console.WriteLine("Compilacao terminada em " & cronometro.ElapsedMilliseconds & " mili Segundos")

        Return 0
    End Function


    Private Sub sanitizador_Gold_Parser(ByRef str As StringBuilder)
        Dim input_ch As String() = New String() {"ç", "Ç", "á", "é", "í", "ó", "ú", "ý", "Á", "É", "Í", "Ó", "Ú", "Ý", "à", "è", "ì", "ò", "ù", "À", "È", "Ì", "Ò", "Ù", "ã", "õ", "ñ", "ä", "ë", "ï", "ö", "ü", "ÿ", "Ä", "Ë", "Ï", "Ö", "Ü", "Ã", "Õ", "Ñ", "â", "ê", "î", "ô", "û", "Â", "Ê", "Î", "Ô", "Û", "ª", "º"}
        Dim output_c As String() = New String() {"c", "C", "a", "e", "i", "o", "u", "y", "A", "E", "I", "O", "U", "Y", "a", "e", "i", "o", "u", "A", "E", "I", "O", "U", "a", "o", "n", "a", "e", "i", "o", "u", "y", "A", "E", "I", "O", "U", "A", "O", "N", "a", "e", "i", "o", "u", "A", "E", "I", "O", "U", "a", "o"}

        For i As Integer = 0 To input_ch.Length - 1
            str = str.Replace(input_ch(i), output_c(i))
        Next
    End Sub

    Public Function Adicionar_a_lista_compilacao(ByVal code_path As String) As Boolean

        Dim p_adicionar As New Buffer_Entrada_codigo_fonte_S

        If System.IO.File.Exists(code_path) Then
            p_adicionar.caminho = code_path
            p_adicionar.nome = Path.GetFileNameWithoutExtension(code_path)
            p_adicionar.cd_lido = False

            codigo_fonte_lst.Add(p_adicionar)
        Else
            Return False
        End If

        Return True

    End Function

End Module
