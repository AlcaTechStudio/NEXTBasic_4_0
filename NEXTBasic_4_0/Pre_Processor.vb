Imports System.Diagnostics.Eventing
Imports System.IO
Imports GOLD

Module Pre_Processor

    Private buffer_modificadores As New List(Of String)
    Private Global_preprocessor_fail As Boolean
    Private Global_current_reader As Buffer_Entrada_codigo_fonte_S

    Public Function preprocessor_Apply(ByRef Reader As Buffer_Entrada_codigo_fonte_S) As Boolean
        Dim apply_ok As Boolean = False







        Return apply_ok
    End Function

    Public Function Preprocessor_Scan(ByRef Reader As Buffer_Entrada_codigo_fonte_S) As Boolean
        'This procedure starts the GOLD Parser Engine and handles each of the
        'messages it returns. Each time a reduction is made, you can create new
        'custom object and reassign the .CurrentReduction property. Otherwise, 
        'the system will use the Reduction object that was returned.
        '
        'The resulting tree will be a pure representation of the language 
        'and will be ready to implement.

        Dim Response As GOLD.ParseMessage
        Dim Done As Boolean                  'Controls when we leave the loop
        Dim Accepted As Boolean = False      'Was the parse successful?


        Parser.Open(Reader.codigo.ToString())
        Parser.TrimReductions = False  'Please read about this feature before enabling  
        buffer_modificadores.Clear()

        Done = False
        Global_preprocessor_fail = False
        Global_current_reader = Reader

        Do Until Done
            Response = Parser.Parse()
            If Global_preprocessor_fail Then Exit Do
            Select Case Response
                Case GOLD.ParseMessage.LexicalError
                    'Cannot recognize token
                    Console.WriteLine("Erro: identificador nao reconhecido: " & Parser.CurrentToken.Data.ToString() & " linha: '" & Parser.CurrentToken.Position.Line & "' Coluna: '" & Parser.CurrentToken.Position.Column & Parser.CurrentToken.Data.ToString() & System.Environment.NewLine)
                    Console.WriteLine(Pega_linha_source_file(Parser.CurrentToken.Position.Line, Reader.caminho))
                    Done = True

                Case GOLD.ParseMessage.SyntaxError
                    'Expecting a different token
                    Console.WriteLine("Erro: Esperando identificador diferente '" & Parser.CurrentToken.Data.ToString() & "' Coluna: ' " & Parser.CurrentToken.Position.Column & " ' " & Parser.CurrentToken.Data.ToString() & System.Environment.NewLine)
                    Console.WriteLine(Pega_linha_source_file(Parser.CurrentToken.Position.Line, Reader.caminho)) '"Linha: " & basic_source_lines(Parser.CurrentToken.Position.Line))
                    Done = True

                Case GOLD.ParseMessage.Reduction
                    'Create a customized object to store the reduction
                    'Dim CurrentReduction As Object = Preprocessor_reduce(Parser.CurrentReduction)

                Case GOLD.ParseMessage.Accept
                    'Accepted!
                    'Program = Parser.CurrentReduction  'The root node!                 
                    Done = True
                    Accepted = True
                    If Parser.CurrentReduction IsNot Nothing Then
                        Try
                            Preprocessor_reduce(Parser.CurrentReduction)
                        Catch ex As Exception
                            Console.WriteLine(ex.ToString())
                        End Try
                    End If
                Case GOLD.ParseMessage.TokenRead
                    'You don't have to do anything here.

                Case GOLD.ParseMessage.InternalError
                    'INTERNAL ERROR! Something is horribly wrong.
                    Console.WriteLine("INTERNAL ERROR! Something is horribly wrong.")
                    Done = True

                Case GOLD.ParseMessage.NotLoadedError
                    'This error occurs if the CGT was not loaded.
                    Console.WriteLine("This error occurs if the CGT was not loaded.")
                    Done = True

                Case GOLD.ParseMessage.GroupError
                    'COMMENT ERROR! Unexpected end of file
                    Console.WriteLine("COMMENT ERROR! Unexpected end of file")
                    Done = True
            End Select
        Loop

        If Accepted Then
            Reader.modificadores = buffer_modificadores
        End If

        Return Accepted
    End Function


    Private Function Preprocessor_reduce(Reduction As GOLD.Reduction) As Object
        Dim Result As Object = Nothing

        With Reduction
            Select Case .Parent.TableIndex
                Case ProductionIndex.Nl_Newline
                    ' <nl> ::= NewLine <nl> 
                    Return vbNull
                Case ProductionIndex.Nl_Newline2
                    ' <nl> ::= NewLine 
                    Return Nothing
                Case ProductionIndex.Nlopt_Newline
                    ' <nl Opt> ::= NewLine <nl Opt> 
                    Return Nothing
                Case ProductionIndex.Nlopt
                    ' <nl Opt> ::=  
                    Return Nothing
                Case ProductionIndex.Program
                    ' <Program> ::= <nl Opt> <Statements> 
                    Preprocessor_reduce(Reduction(1).Data)
                Case ProductionIndex.Statements
                    ' <Statements> ::= <Statement> <Statements> 
                    Preprocessor_reduce(Reduction(0).Data)
                    Preprocessor_reduce(Reduction(1).Data)
                Case ProductionIndex.Statements2
                    ' <Statements> ::=  
                    Return Nothing
                Case ProductionIndex.Statement
                    ' <Statement> ::= <If Stm> 
                    Preprocessor_reduce(Reduction(0).Data)
                Case ProductionIndex.Statement2
                    ' <Statement> ::= <Select Stm> 
                    Preprocessor_reduce(Reduction(0).Data)
                Case ProductionIndex.Statement_Numconst_Id_Eq
                    ' <Statement> ::= '#const' ID '=' <Expression> <nl> 
                    'TODO
                Case ProductionIndex.Statement_Numlog_Stringliteral
                    ' <Statement> ::= '#Log' StringLiteral <nl> 
                    Return Nothing
                Case ProductionIndex.Statement_Numcompdir_Stringliteral
                    ' <Statement> ::= '#compdir' StringLiteral <nl> 
                    Return Nothing
                Case ProductionIndex.Statement_Num_Numendif
                    ' <Statement> ::= '#' <cond_comp_beg> <Expression> <nl> <Statements> <cond_comp_block> '#endif' <nl> 
                    'TODO
                    Return Nothing
                Case ProductionIndex.Statement_For_Eq_To
                    ' <Statement> ::= For <Variable> '=' <Expression> To <Expression> <nl> <Statements> <next_fn> <nl> 
                    Preprocessor_reduce(Reduction(7).Data)
                Case ProductionIndex.Statement_For_Eq_To_Step
                    ' <Statement> ::= For <Variable> '=' <Expression> To <Expression> step <Expression> <nl> <Statements> <next_fn> <nl> 
                    Preprocessor_reduce(Reduction(9).Data)
                Case ProductionIndex.Statement_Do_Loop
                    ' <Statement> ::= Do <nl> <Statements> loop <nl> 
                    Preprocessor_reduce(Reduction(2).Data)
                Case ProductionIndex.Statement_While_Lparen_Rparen
                    ' <Statement> ::= While '(' <Expression> ')' <nl> <Statements> <while_end> <nl> 
                    Preprocessor_reduce(Reduction(5).Data)
                Case ProductionIndex.Statement_While_Lparen_Rparen2
                    ' <Statement> ::= While '(' <Expression> ')' <Non-Block Stm> <nl> 
                    Return Nothing
                Case ProductionIndex.Statement_Sysreg_Id_As_At_Hexliteral
                    ' <Statement> ::= sysreg ID as <var_type> at hexLiteral <nl> 
                    Dim _error_ As String = declarar_variavel(Reduction(1).Data.ToString(), Preprocessor_reduce(Reduction(3).Data), 1, True, "", 0, Reduction(5).Data.ToString().Substring(2))
                    If Not (String.IsNullOrEmpty(_error_)) Then
                        Console.WriteLine(_error_)
                        Console.WriteLine(Pega_linha_source_file(Reduction(0).Position.Line, Global_current_reader.caminho))
                    End If
                Case ProductionIndex.Statement_Id_As
                    ' <Statement> ::= <dim_g> ID <var_opt_param> as <var_type> <var_opt_assing> <nl> 
                    'TODO
                Case ProductionIndex.Statement_Structure_Id_End_Structure
                    ' <Statement> ::= Structure ID <nl> <Enum List> End Structure <nl> 
                    'TODO
                Case ProductionIndex.Statement3
                    ' <Statement> ::= <function_dec_s> <nl> 
                    'TODO
                Case ProductionIndex.Statement4
                    ' <Statement> ::= <sub_dec_s> <nl> 
                    'TODO
                Case ProductionIndex.Statement_Imports_Stringliteral
                    ' <Statement> ::= imports StringLiteral <nl> 
                    Dim path_str As String = Reduction(1).Data.ToString().Substring(1, Reduction(1).Data.ToString.Length - 2).ToLower()
                    If path_str.Contains(",") Then path_str = path_str.Substring(0, path_str.IndexOf(","))

                    If path_str.Contains("\system\") Then
                        path_str = AppDomain.CurrentDomain.BaseDirectory & path_str.Substring(1)
                    Else
                        path_str = Path.Combine(pasta_projeto, path_str)
                    End If

                    If File.Exists(path_str) Then
                        Dim tipo As String = Path.GetExtension(path_str)
                        If String.Compare(tipo, ".nbh", True) = 0 Or String.Compare(tipo, ".nbs", True) = 0 Then
                            ' Limpar essa linha????
                            Adicionar_a_lista_compilacao(path_str)
                        End If
                    Else
                        Console.WriteLine("Erro de compilacao, arquivo nao encontrado: " & Path.GetFileName(path_str))
                        Console.WriteLine(Pega_linha_source_file(Reduction(0).Position.Line, Global_current_reader.caminho))
                        Global_preprocessor_fail = True
                    End If
                Case ProductionIndex.Statement_Equal_Id_Stringliteral
                    ' <Statement> ::= Equal ID StringLiteral <nl> 

                Case ProductionIndex.Statement_Label
                    ' <Statement> ::= LABEL <nl> 
                    Dim _error_ As String = declarar_label(Reduction(0).Data.ToString())
                    If Not (String.IsNullOrEmpty(_error_)) Then
                        Console.WriteLine(_error_)
                        Console.WriteLine(Pega_linha_source_file(Reduction(0).Position.Line, Global_current_reader.caminho))
                    End If
                Case ProductionIndex.Statement_Data
                    ' <Statement> ::= Data <data list> <nl> 

                Case ProductionIndex.Statement_Dataint
                    ' <Statement> ::= Dataint <data list> <nl> 

                Case ProductionIndex.Statement_Datalong
                    ' <Statement> ::= Datalong <data list> <nl> 

                Case ProductionIndex.Statement_Asmliteral
                    ' <Statement> ::= asmLiteral <nl> 

                Case ProductionIndex.Statement5
                    ' <Statement> ::= <Non-Block Stm> <nl> 
                    Preprocessor_reduce(Reduction(0).Data)
                Case ProductionIndex.Nonblockstm_Exit_Do
                    ' <Non-Block Stm> ::= Exit Do 

                Case ProductionIndex.Nonblockstm_Exit_For
                    ' <Non-Block Stm> ::= Exit For 

                Case ProductionIndex.Nonblockstm_Exit_While
                    ' <Non-Block Stm> ::= Exit While 

                Case ProductionIndex.Nonblockstm_Exit_Select
                    ' <Non-Block Stm> ::= Exit Select 

                Case ProductionIndex.Nonblockstm_Goto_Id
                    ' <Non-Block Stm> ::= GoTo ID 

                Case ProductionIndex.Nonblockstm__asm_Lparen_Stringliteral_Rparen
                    ' <Non-Block Stm> ::= '_asm' '(' StringLiteral ')' 

                Case ProductionIndex.Nonblockstm_Return
                    ' <Non-Block Stm> ::= Return 

                Case ProductionIndex.Nonblockstm_Return2
                    ' <Non-Block Stm> ::= Return <Expression> 

                Case ProductionIndex.Nonblockstm_Bit_set_Lparen_Comma_Rparen
                    ' <Non-Block Stm> ::= 'Bit_Set' '(' <Variable> ',' <Expression> ')' 

                Case ProductionIndex.Nonblockstm_Bit_clear_Lparen_Comma_Rparen
                    ' <Non-Block Stm> ::= 'Bit_Clear' '(' <Variable> ',' <Expression> ')' 

                Case ProductionIndex.Nonblockstm_Push_Lparen_As_Comma_Stringliteral_Rparen
                    ' <Non-Block Stm> ::= push '(' <Expression> as <reg_size> ',' StringLiteral ')' 

                Case ProductionIndex.Nonblockstm_Poke_Lparen_As_Comma_Rparen
                    ' <Non-Block Stm> ::= poke '(' <Expression> as <reg_size> ',' <Expression> ')' 

                Case ProductionIndex.Nonblockstm
                    ' <Non-Block Stm> ::= <Variable> <Assign Op> <Expression> 

                Case ProductionIndex.Nonblockstm2
                    ' <Non-Block Stm> ::= <sub_call> 

                Case ProductionIndex.Function_dec_s_Function_Id_As_End_Function
                    ' <function_dec_s> ::= <opt_inline> Function ID <Param List> as <var_type> <nl> <Statements> End Function 

                Case ProductionIndex.Sub_dec_s_Sub_Id_End_Sub
                    ' <sub_dec_s> ::= <opt_inline> Sub ID <Param List> <nl> <Statements> End Sub 

                Case ProductionIndex.Var_opt_param_Lbracket_Rbracket
                    ' <var_opt_param> ::= '[' <var_Argument_dec> ']' 

                Case ProductionIndex.Var_opt_param_Lbracket_Rbracket2
                    ' <var_opt_param> ::= '[' ']' 

                Case ProductionIndex.Var_opt_param
                    ' <var_opt_param> ::=  

                Case ProductionIndex.Var_opt_assing_Eq
                    ' <var_opt_assing> ::= '=' <Expression> 

                Case ProductionIndex.Var_opt_assing
                    ' <var_opt_assing> ::=  

                Case ProductionIndex.While_end_End_While
                    ' <while_end> ::= End While 

                Case ProductionIndex.While_end_Wend
                    ' <while_end> ::= wend 

                Case ProductionIndex.Assignop_Eq
                    ' <Assign Op> ::= '=' 

                Case ProductionIndex.Assignop_Timeseq
                    ' <Assign Op> ::= '*=' 

                Case ProductionIndex.Assignop_Diveq
                    ' <Assign Op> ::= '/=' 

                Case ProductionIndex.Assignop_Backslasheq
                    ' <Assign Op> ::= '\=' 

                Case ProductionIndex.Assignop_Pluseq
                    ' <Assign Op> ::= '+=' 

                Case ProductionIndex.Assignop_Minuseq
                    ' <Assign Op> ::= '-=' 

                Case ProductionIndex.Assignop_Ltlteq
                    ' <Assign Op> ::= '<<=' 

                Case ProductionIndex.Assignop_Gtgteq
                    ' <Assign Op> ::= '>>=' 

                Case ProductionIndex.Assignop_Ampeq
                    ' <Assign Op> ::= '&=' 

                Case ProductionIndex.Assignop_Pipeeq
                    ' <Assign Op> ::= '|=' 

                Case ProductionIndex.Assignop_Careteq
                    ' <Assign Op> ::= '^=' 

                Case ProductionIndex.Assignop_Tildeeq
                    ' <Assign Op> ::= '~=' 

                Case ProductionIndex.Next_fn_Next
                    ' <next_fn> ::= next 

                Case ProductionIndex.Next_fn_Next_Id
                    ' <next_fn> ::= next ID 

                Case ProductionIndex.Dim_g_Dim
                    ' <dim_g> ::= dim 

                Case ProductionIndex.Dim_g_Global
                    ' <dim_g> ::= global 

                Case ProductionIndex.Opt_inline_Inline
                    ' <opt_inline> ::= inline 

                Case ProductionIndex.Opt_inline
                    ' <opt_inline> ::=  

                Case ProductionIndex.Cond_comp_beg_If
                    ' <cond_comp_beg> ::= if 

                Case ProductionIndex.Cond_comp_beg_Ifdef
                    ' <cond_comp_beg> ::= ifdef 

                Case ProductionIndex.Cond_comp_beg_Ifndef
                    ' <cond_comp_beg> ::= ifndef 

                Case ProductionIndex.Cond_comp_block_Numelse
                    ' <cond_comp_block> ::= '#Else' <cond_comp_beg> <Expression> <nl> <Statements> <cond_comp_block> 

                Case ProductionIndex.Cond_comp_block_Numelse2
                    ' <cond_comp_block> ::= '#Else' <nl> <Statements> 

                Case ProductionIndex.Cond_comp_block
                    ' <cond_comp_block> ::=  

                Case ProductionIndex.Ifstm_If_Then
                    ' <If Stm> ::= if <Expression> Then <nl> <Statements> <If Blocks> <if_e> <nl> 

                Case ProductionIndex.Ifstm_If_Then2
                    ' <If Stm> ::= if <Expression> Then <Non-Block Stm> <nl> 

                Case ProductionIndex.Ifstm_If_Then_Else
                    ' <If Stm> ::= if <Expression> Then <Non-Block Stm> Else <Non-Block Stm> <nl> 

                Case ProductionIndex.Ifblocks_Then
                    ' <If Blocks> ::= <else_if> <Expression> Then <nl> <Statements> <If Blocks> 

                Case ProductionIndex.Ifblocks_Else
                    ' <If Blocks> ::= Else <nl> <Statements> 

                Case ProductionIndex.Ifblocks
                    ' <If Blocks> ::=  

                Case ProductionIndex.If_e_End_If
                    ' <if_e> ::= End if 

                Case ProductionIndex.If_e_Endif
                    ' <if_e> ::= endif 

                Case ProductionIndex.Else_if_Elseif
                    ' <else_if> ::= Elseif 

                Case ProductionIndex.Else_if_Else_If
                    ' <else_if> ::= Else if 

                Case ProductionIndex.Var_type_Byte
                    ' <var_type> ::= <opt_var_attb> Byte 
                    Return Preprocessor_reduce(Reduction(0).Data) + Data_type_ubyte
                Case ProductionIndex.Var_type_Word
                    ' <var_type> ::= <opt_var_attb> word 
                    Return Preprocessor_reduce(Reduction(0).Data) + Data_type_uword
                Case ProductionIndex.Var_type_Long
                    ' <var_type> ::= <opt_var_attb> long 
                    Return Preprocessor_reduce(Reduction(0).Data) + Data_type_ulong
                Case ProductionIndex.Var_type_Integer
                    ' <var_type> ::= <opt_var_attb> integer 
                    Return Preprocessor_reduce(Reduction(0).Data) + Data_type_uword
                Case ProductionIndex.Var_type_String
                    ' <var_type> ::= string 
                    Return Data_type_uword
                Case ProductionIndex.Var_type_Fixed
                    ' <var_type> ::= <opt_var_attb> fixed 
                    Return Preprocessor_reduce(Reduction(0).Data) + Data_type_ufixed
                Case ProductionIndex.Var_type_Single
                    ' <var_type> ::= <opt_var_attb> single 
                    Return Preprocessor_reduce(Reduction(0).Data) + Data_type_usingle
                Case ProductionIndex.Var_type_New_Id
                    ' <var_type> ::= new ID 

                Case ProductionIndex.Var_type_Id
                    ' <var_type> ::= ID 

                Case ProductionIndex.Opt_var_attb_Signed
                    ' <opt_var_attb> ::= signed 
                    Return 1
                Case ProductionIndex.Opt_var_attb_Unsigned
                    ' <opt_var_attb> ::= unsigned 
                    Return 0
                Case ProductionIndex.Opt_var_attb
                    ' <opt_var_attb> ::=  
                    Return 0
                Case ProductionIndex.Enumlist
                    ' <Enum List> ::= <Enum Item> <Enum List> 

                Case ProductionIndex.Enumlist2
                    ' <Enum List> ::=  

                Case ProductionIndex.Enumitem_Dim_Id_As
                    ' <Enum Item> ::= dim ID as <var_type> <nl> 

                Case ProductionIndex.Enumitem
                    ' <Enum Item> ::= <sub_dec_s> <nl> 

                Case ProductionIndex.Enumitem2
                    ' <Enum Item> ::= <function_dec_s> <nl> 

                Case ProductionIndex.Selectstm_Select_End_Select
                    ' <Select Stm> ::= Select <Expression> <nl> <Select Blocks> End Select <nl> 

                Case ProductionIndex.Selectblocks_Case
                    ' <Select Blocks> ::= Case <Expression> <nl> <Statements> <Select Blocks> 

                Case ProductionIndex.Selectblocks_Case_Else
                    ' <Select Blocks> ::= Case Else <nl> <Statements> 

                Case ProductionIndex.Selectblocks
                    ' <Select Blocks> ::=  

                Case ProductionIndex.Expression_Or
                    ' <Expression> ::= <And Exp> Or <Expression> 

                Case ProductionIndex.Expression_Xor
                    ' <Expression> ::= <And Exp> XOr <Expression> 

                Case ProductionIndex.Expression
                    ' <Expression> ::= <And Exp> 

                Case ProductionIndex.Andexp_And
                    ' <And Exp> ::= <Not Exp> And <And Exp> 

                Case ProductionIndex.Andexp
                    ' <And Exp> ::= <Not Exp> 

                Case ProductionIndex.Notexp_Not
                    ' <Not Exp> ::= NOT <Compare Exp> 

                Case ProductionIndex.Notexp
                    ' <Not Exp> ::= <Compare Exp> 

                Case ProductionIndex.Compareexp
                    ' <Compare Exp> ::= <Shift Exp> <Compare Op> <Compare Exp> 

                Case ProductionIndex.Compareexp2
                    ' <Compare Exp> ::= <Shift Exp> 

                Case ProductionIndex.Shiftexp_Ltlt
                    ' <Shift Exp> ::= <Add Exp> '<<' <Shift Exp> 

                Case ProductionIndex.Shiftexp_Gtgt
                    ' <Shift Exp> ::= <Add Exp> '>>' <Shift Exp> 

                Case ProductionIndex.Shiftexp
                    ' <Shift Exp> ::= <Add Exp> 

                Case ProductionIndex.Addexp_Plus
                    ' <Add Exp> ::= <Modulus Exp> '+' <Add Exp> 

                Case ProductionIndex.Addexp_Minus
                    ' <Add Exp> ::= <Modulus Exp> '-' <Add Exp> 

                Case ProductionIndex.Addexp
                    ' <Add Exp> ::= <Modulus Exp> 

                Case ProductionIndex.Modulusexp_Mod
                    ' <Modulus Exp> ::= <Int Div Exp> Mod <Modulus Exp> 

                Case ProductionIndex.Modulusexp
                    ' <Modulus Exp> ::= <Int Div Exp> 

                Case ProductionIndex.Intdivexp_Backslash
                    ' <Int Div Exp> ::= <Mult Exp> '\' <Int Div Exp> 

                Case ProductionIndex.Intdivexp
                    ' <Int Div Exp> ::= <Mult Exp> 

                Case ProductionIndex.Multexp_Times
                    ' <Mult Exp> ::= <Negate Exp> '*' <Mult Exp> 

                Case ProductionIndex.Multexp_Div
                    ' <Mult Exp> ::= <Negate Exp> '/' <Mult Exp> 

                Case ProductionIndex.Multexp
                    ' <Mult Exp> ::= <Negate Exp> 

                Case ProductionIndex.Negateexp_Minus
                    ' <Negate Exp> ::= '-' <Value> 

                Case ProductionIndex.Negateexp_Tilde
                    ' <Negate Exp> ::= '~' <Value> 

                Case ProductionIndex.Negateexp
                    ' <Negate Exp> ::= <Value> 

                Case ProductionIndex.Value_Lparen_Rparen
                    ' <Value> ::= '(' <Expression> ')' 

                Case ProductionIndex.Value_Decliteral
                    ' <Value> ::= decLiteral 

                Case ProductionIndex.Value_Fixliteral
                    ' <Value> ::= fixliteral 

                Case ProductionIndex.Value_Hexliteral
                    ' <Value> ::= hexLiteral 

                Case ProductionIndex.Value_Binliteral
                    ' <Value> ::= binLiteral 

                Case ProductionIndex.Value_Chrliteral
                    ' <Value> ::= chrliteral 

                Case ProductionIndex.Value_Stringliteral
                    ' <Value> ::= StringLiteral 

                Case ProductionIndex.Value_True
                    ' <Value> ::= True 

                Case ProductionIndex.Value_False
                    ' <Value> ::= False 

                Case ProductionIndex.Value_Addressof_Lparen_Rparen
                    ' <Value> ::= AddressOf '(' <Variable> ')' 

                Case ProductionIndex.Value_Pop_Lparen_Stringliteral_As_Rparen
                    ' <Value> ::= pop '(' StringLiteral as <reg_size> ')' 

                Case ProductionIndex.Value_Peek_Lparen_As_Rparen
                    ' <Value> ::= peek '(' <Expression> as <reg_size> ')' 

                Case ProductionIndex.Value_Bit_test_Lparen_Comma_Rparen
                    ' <Value> ::= 'Bit_Test' '(' <Expression> ',' <Expression> ')' 

                Case ProductionIndex.Value_Id_Dot_Decliteral
                    ' <Value> ::= ID '.' decLiteral 

                Case ProductionIndex.Value
                    ' <Value> ::= <Variable> 

                Case ProductionIndex.Value2
                    ' <Value> ::= <func_call> 

                Case ProductionIndex.Value_Lbracket_Rbracket
                    ' <Value> ::= '[' <data list> ']' 

                Case ProductionIndex.Variable_Id_Lbracket_Rbracket
                    ' <Variable> ::= ID '[' <var_Argument> ']' 

                Case ProductionIndex.Variable_Id_Lbracket_Rbracket_Dot_Id
                    ' <Variable> ::= ID '[' <var_Argument> ']' '.' ID 

                Case ProductionIndex.Variable_Id_Dot_Id
                    ' <Variable> ::= ID '.' ID 

                Case ProductionIndex.Variable_Id
                    ' <Variable> ::= ID 

                Case ProductionIndex.Func_call
                    ' <func_call> ::= <Variable> <Argument List_F> 

                Case ProductionIndex.Sub_call
                    ' <sub_call> ::= <Variable> <Argument List_F> 

                Case ProductionIndex.Compareop_Eq
                    ' <Compare Op> ::= '=' 

                Case ProductionIndex.Compareop_Ltgt
                    ' <Compare Op> ::= '<>' 

                Case ProductionIndex.Compareop_Lt
                    ' <Compare Op> ::= '<' 

                Case ProductionIndex.Compareop_Gt
                    ' <Compare Op> ::= '>' 

                Case ProductionIndex.Compareop_Gteq
                    ' <Compare Op> ::= '>=' 

                Case ProductionIndex.Compareop_Lteq
                    ' <Compare Op> ::= '<=' 

                Case ProductionIndex.Datalist_Comma
                    ' <data list> ::= <data> ',' <data list> 

                Case ProductionIndex.Datalist
                    ' <data list> ::= <data> 

                Case ProductionIndex.Data_Decliteral
                    ' <data> ::= decLiteral 

                Case ProductionIndex.Data_Fixliteral
                    ' <data> ::= fixliteral 

                Case ProductionIndex.Data_Minus_Decliteral
                    ' <data> ::= '-' decLiteral 

                Case ProductionIndex.Data_Minus_Fixliteral
                    ' <data> ::= '-' fixliteral 

                Case ProductionIndex.Data_Hexliteral
                    ' <data> ::= hexLiteral 

                Case ProductionIndex.Data_Binliteral
                    ' <data> ::= binLiteral 

                Case ProductionIndex.Data_Stringliteral
                    ' <data> ::= StringLiteral 

                Case ProductionIndex.Data_Chrliteral
                    ' <data> ::= chrliteral 

                Case ProductionIndex.Data_Id
                    ' <data> ::= ID 

                Case ProductionIndex.Var_argument_dec_Comma
                    ' <var_Argument_dec> ::= <var_arg_dec> ',' <var_Argument_dec> 

                Case ProductionIndex.Var_argument_dec
                    ' <var_Argument_dec> ::= <var_arg_dec> 

                Case ProductionIndex.Var_arg_dec_Decliteral
                    ' <var_arg_dec> ::= decLiteral 

                Case ProductionIndex.Var_arg_dec_Hexliteral
                    ' <var_arg_dec> ::= hexLiteral 

                Case ProductionIndex.Var_arg_dec_Binliteral
                    ' <var_arg_dec> ::= binLiteral 

                Case ProductionIndex.Var_arg_dec_Chrliteral
                    ' <var_arg_dec> ::= chrliteral 

                Case ProductionIndex.Var_argument_Comma
                    ' <var_Argument> ::= <var_arg> ',' <var_Argument> 

                Case ProductionIndex.Var_argument
                    ' <var_Argument> ::= <var_arg> 

                Case ProductionIndex.Var_arg
                    ' <var_arg> ::= <Expression> 

                Case ProductionIndex.Argumentlist_f_Lparen_Rparen
                    ' <Argument List_F> ::= '(' <Argument Items_F> ')' 

                Case ProductionIndex.Argumentlist_f_Lparen_Rparen2
                    ' <Argument List_F> ::= '(' ')' 

                Case ProductionIndex.Argumentitems_f_Comma
                    ' <Argument Items_F> ::= <Expression> ',' <Argument Items_F> 

                Case ProductionIndex.Argumentitems_f
                    ' <Argument Items_F> ::= <Expression> 

                Case ProductionIndex.Paramlist_Lparen_Rparen
                    ' <Param List> ::= '(' <Param Items> ')' 

                Case ProductionIndex.Paramlist_Lparen_Rparen2
                    ' <Param List> ::= '(' ')' 

                Case ProductionIndex.Paramitems_Comma
                    ' <Param Items> ::= <Param Item> ',' <Param Items> 

                Case ProductionIndex.Paramitems
                    ' <Param Items> ::= <Param Item> 

                Case ProductionIndex.Paramitem_Byval_Id_As
                    ' <Param Item> ::= ByVal ID as <var_type> 

                Case ProductionIndex.Paramitem_Byref_Id_As
                    ' <Param Item> ::= ByRef ID as <var_type> 

                Case ProductionIndex.Reg_size_Byte
                    ' <reg_size> ::= Byte 

                Case ProductionIndex.Reg_size_Word
                    ' <reg_size> ::= word 

                Case ProductionIndex.Reg_size_Integer
                    ' <reg_size> ::= integer 

                Case ProductionIndex.Reg_size_Long
                    ' <reg_size> ::= long 

            End Select
        End With

        Return Result
    End Function
End Module
