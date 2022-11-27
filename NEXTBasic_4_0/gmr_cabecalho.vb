Module gmr_cabecalho
    Public Enum SymbolIndex
        [Eof] = 0                                 ' (EOF)
        [Error] = 1                               ' (Error)
        [Comment_line] = 2                        ' 'Comment_Line'
        [Whitespace] = 3                          ' Whitespace
        [Minus] = 4                               ' '-'
        [Num] = 5                                 ' '#'
        [Numcompdir] = 6                          ' '#compdir'
        [Numconst] = 7                            ' '#const'
        [Numelse] = 8                             ' '#Else'
        [Numendif] = 9                            ' '#endif'
        [Numlog] = 10                             ' '#Log'
        [Ampeq] = 11                              ' '&='
        [Lparen] = 12                             ' '('
        [Rparen] = 13                             ' ')'
        [Times] = 14                              ' '*'
        [Timeseq] = 15                            ' '*='
        [Comma] = 16                              ' ','
        [Dot] = 17                                ' '.'
        [Div] = 18                                ' '/'
        [Diveq] = 19                              ' '/='
        [Lbracket] = 20                           ' '['
        [Backslash] = 21                          ' '\'
        [Backslasheq] = 22                        ' '\='
        [Rbracket] = 23                           ' ']'
        [Careteq] = 24                            ' '^='
        [_asm] = 25                               ' '_asm'
        [Pipeeq] = 26                             ' '|='
        [Tilde] = 27                              ' '~'
        [Tildeeq] = 28                            ' '~='
        [Plus] = 29                               ' '+'
        [Pluseq] = 30                             ' '+='
        [Lt] = 31                                 ' '<'
        [Ltlt] = 32                               ' '<<'
        [Ltlteq] = 33                             ' '<<='
        [Lteq] = 34                               ' '<='
        [Ltgt] = 35                               ' '<>'
        [Eq] = 36                                 ' '='
        [Minuseq] = 37                            ' '-='
        [Gt] = 38                                 ' '>'
        [Gteq] = 39                               ' '>='
        [Gtgt] = 40                               ' '>>'
        [Gtgteq] = 41                             ' '>>='
        [Addressof] = 42                          ' AddressOf
        [And] = 43                                ' And
        [As] = 44                                 ' as
        [Asmliteral] = 45                         ' asmLiteral
        [At] = 46                                 ' at
        [Binliteral] = 47                         ' binLiteral
        [Bit_clear] = 48                          ' 'Bit_Clear'
        [Bit_set] = 49                            ' 'Bit_Set'
        [Bit_test] = 50                           ' 'Bit_Test'
        [Byref] = 51                              ' ByRef
        [Byte] = 52                               ' Byte
        [Byval] = 53                              ' ByVal
        [Case] = 54                               ' Case
        [Chrliteral] = 55                         ' chrliteral
        [Data] = 56                               ' Data
        [Dataint] = 57                            ' Dataint
        [Datalong] = 58                           ' Datalong
        [Decliteral] = 59                         ' decLiteral
        [Dim] = 60                                ' dim
        [Do] = 61                                 ' Do
        [Else] = 62                               ' Else
        [Elseif] = 63                             ' Elseif
        [End] = 64                                ' End
        [Endif] = 65                              ' endif
        [Equal] = 66                              ' Equal
        [Exit] = 67                               ' Exit
        [False] = 68                              ' False
        [Fixed] = 69                              ' fixed
        [Fixliteral] = 70                         ' fixliteral
        [For] = 71                                ' For
        [Function] = 72                           ' Function
        [Global] = 73                             ' global
        [Goto] = 74                               ' GoTo
        [Hexliteral] = 75                         ' hexLiteral
        [Id] = 76                                 ' ID
        [If] = 77                                 ' if
        [Ifdef] = 78                              ' ifdef
        [Ifndef] = 79                             ' ifndef
        [Imports] = 80                            ' imports
        [Inline] = 81                             ' inline
        [Integer] = 82                            ' integer
        [Label] = 83                              ' LABEL
        [Long] = 84                               ' long
        [Loop] = 85                               ' loop
        [Mod] = 86                                ' Mod
        [New] = 87                                ' new
        [Newline] = 88                            ' NewLine
        [Next] = 89                               ' next
        [Not] = 90                                ' NOT
        [Or] = 91                                 ' Or
        [Peek] = 92                               ' peek
        [Poke] = 93                               ' poke
        [Pop] = 94                                ' pop
        [Push] = 95                               ' push
        [Return] = 96                             ' Return
        [Select] = 97                             ' Select
        [Signed] = 98                             ' signed
        [Single] = 99                             ' single
        [Step] = 100                              ' step
        [String] = 101                            ' string
        [Stringliteral] = 102                     ' StringLiteral
        [Structure] = 103                         ' Structure
        [Sub] = 104                               ' Sub
        [Sysreg] = 105                            ' sysreg
        [Then] = 106                              ' Then
        [To] = 107                                ' To
        [True] = 108                              ' True
        [Unsigned] = 109                          ' unsigned
        [Wend] = 110                              ' wend
        [While] = 111                             ' While
        [Word] = 112                              ' word
        [Xor] = 113                               ' XOr
        [Addexp] = 114                            ' <Add Exp>
        [Andexp] = 115                            ' <And Exp>
        [Argumentitems_f] = 116                   ' <Argument Items_F>
        [Argumentlist_f] = 117                    ' <Argument List_F>
        [Assignop] = 118                          ' <Assign Op>
        [Compareexp] = 119                        ' <Compare Exp>
        [Compareop] = 120                         ' <Compare Op>
        [Cond_comp_beg] = 121                     ' <cond_comp_beg>
        [Cond_comp_block] = 122                   ' <cond_comp_block>
        [Data2] = 123                             ' <data>
        [Datalist] = 124                          ' <data list>
        [Dim_g] = 125                             ' <dim_g>
        [Else_if] = 126                           ' <else_if>
        [Enumitem] = 127                          ' <Enum Item>
        [Enumlist] = 128                          ' <Enum List>
        [Expression] = 129                        ' <Expression>
        [Func_call] = 130                         ' <func_call>
        [Function_dec_s] = 131                    ' <function_dec_s>
        [Ifblocks] = 132                          ' <If Blocks>
        [Ifstm] = 133                             ' <If Stm>
        [If_e] = 134                              ' <if_e>
        [Intdivexp] = 135                         ' <Int Div Exp>
        [Modulusexp] = 136                        ' <Modulus Exp>
        [Multexp] = 137                           ' <Mult Exp>
        [Negateexp] = 138                         ' <Negate Exp>
        [Next_fn] = 139                           ' <next_fn>
        [Nl] = 140                                ' <nl>
        [Nlopt] = 141                             ' <nl Opt>
        [Nonminusblockstm] = 142                  ' <Non-Block Stm>
        [Notexp] = 143                            ' <Not Exp>
        [Opt_inline] = 144                        ' <opt_inline>
        [Opt_var_attb] = 145                      ' <opt_var_attb>
        [Paramitem] = 146                         ' <Param Item>
        [Paramitems] = 147                        ' <Param Items>
        [Paramlist] = 148                         ' <Param List>
        [Program] = 149                           ' <Program>
        [Reg_size] = 150                          ' <reg_size>
        [Selectblocks] = 151                      ' <Select Blocks>
        [Selectstm] = 152                         ' <Select Stm>
        [Shiftexp] = 153                          ' <Shift Exp>
        [Statement] = 154                         ' <Statement>
        [Statements] = 155                        ' <Statements>
        [Sub_call] = 156                          ' <sub_call>
        [Sub_dec_s] = 157                         ' <sub_dec_s>
        [Value] = 158                             ' <Value>
        [Var_arg] = 159                           ' <var_arg>
        [Var_arg_dec] = 160                       ' <var_arg_dec>
        [Var_argument] = 161                      ' <var_Argument>
        [Var_argument_dec] = 162                  ' <var_Argument_dec>
        [Var_opt_assing] = 163                    ' <var_opt_assing>
        [Var_opt_param] = 164                     ' <var_opt_param>
        [Var_type] = 165                          ' <var_type>
        [Variable] = 166                          ' <Variable>
        [While_end] = 167                         ' <while_end>
    End Enum

    Public Enum ProductionIndex
        [Nl_Newline] = 0                          ' <nl> ::= NewLine <nl>
        [Nl_Newline2] = 1                         ' <nl> ::= NewLine
        [Nlopt_Newline] = 2                       ' <nl Opt> ::= NewLine <nl Opt>
        [Nlopt] = 3                               ' <nl Opt> ::= 
        [Program] = 4                             ' <Program> ::= <nl Opt> <Statements>
        [Statements] = 5                          ' <Statements> ::= <Statement> <Statements>
        [Statements2] = 6                         ' <Statements> ::= 
        [Statement] = 7                           ' <Statement> ::= <If Stm>
        [Statement2] = 8                          ' <Statement> ::= <Select Stm>
        [Statement_Numconst_Id_Eq] = 9            ' <Statement> ::= '#const' ID '=' <Expression> <nl>
        [Statement_Numlog_Stringliteral] = 10     ' <Statement> ::= '#Log' StringLiteral <nl>
        [Statement_Numcompdir_Stringliteral] = 11 ' <Statement> ::= '#compdir' StringLiteral <nl>
        [Statement_Num_Numendif] = 12             ' <Statement> ::= '#' <cond_comp_beg> <Expression> <nl> <Statements> <cond_comp_block> '#endif' <nl>
        [Statement_For_Eq_To] = 13                ' <Statement> ::= For <Variable> '=' <Expression> To <Expression> <nl> <Statements> <next_fn> <nl>
        [Statement_For_Eq_To_Step] = 14           ' <Statement> ::= For <Variable> '=' <Expression> To <Expression> step <Expression> <nl> <Statements> <next_fn> <nl>
        [Statement_Do_Loop] = 15                  ' <Statement> ::= Do <nl> <Statements> loop <nl>
        [Statement_While_Lparen_Rparen] = 16      ' <Statement> ::= While '(' <Expression> ')' <nl> <Statements> <while_end> <nl>
        [Statement_While_Lparen_Rparen2] = 17     ' <Statement> ::= While '(' <Expression> ')' <Non-Block Stm> <nl>
        [Statement_Sysreg_Id_As_At_Hexliteral] = 18 ' <Statement> ::= sysreg ID as <var_type> at hexLiteral <nl>
        [Statement_Id_As] = 19                    ' <Statement> ::= <dim_g> ID <var_opt_param> as <var_type> <var_opt_assing> <nl>
        [Statement_Structure_Id_End_Structure] = 20 ' <Statement> ::= Structure ID <nl> <Enum List> End Structure <nl>
        [Statement3] = 21                         ' <Statement> ::= <function_dec_s> <nl>
        [Statement4] = 22                         ' <Statement> ::= <sub_dec_s> <nl>
        [Statement_Imports_Stringliteral] = 23    ' <Statement> ::= imports StringLiteral <nl>
        [Statement_Equal_Id_Stringliteral] = 24   ' <Statement> ::= Equal ID StringLiteral <nl>
        [Statement_Label] = 25                    ' <Statement> ::= LABEL <nl>
        [Statement_Data] = 26                     ' <Statement> ::= Data <data list> <nl>
        [Statement_Dataint] = 27                  ' <Statement> ::= Dataint <data list> <nl>
        [Statement_Datalong] = 28                 ' <Statement> ::= Datalong <data list> <nl>
        [Statement_Asmliteral] = 29               ' <Statement> ::= asmLiteral <nl>
        [Statement5] = 30                         ' <Statement> ::= <Non-Block Stm> <nl>
        [Nonblockstm_Exit_Do] = 31                ' <Non-Block Stm> ::= Exit Do
        [Nonblockstm_Exit_For] = 32               ' <Non-Block Stm> ::= Exit For
        [Nonblockstm_Exit_While] = 33             ' <Non-Block Stm> ::= Exit While
        [Nonblockstm_Exit_Select] = 34            ' <Non-Block Stm> ::= Exit Select
        [Nonblockstm_Goto_Id] = 35                ' <Non-Block Stm> ::= GoTo ID
        [Nonblockstm__asm_Lparen_Stringliteral_Rparen] = 36 ' <Non-Block Stm> ::= '_asm' '(' StringLiteral ')'
        [Nonblockstm_Return] = 37                 ' <Non-Block Stm> ::= Return
        [Nonblockstm_Return2] = 38                ' <Non-Block Stm> ::= Return <Expression>
        [Nonblockstm_Bit_set_Lparen_Comma_Rparen] = 39 ' <Non-Block Stm> ::= 'Bit_Set' '(' <Variable> ',' <Expression> ')'
        [Nonblockstm_Bit_clear_Lparen_Comma_Rparen] = 40 ' <Non-Block Stm> ::= 'Bit_Clear' '(' <Variable> ',' <Expression> ')'
        [Nonblockstm_Push_Lparen_As_Comma_Stringliteral_Rparen] = 41 ' <Non-Block Stm> ::= push '(' <Expression> as <reg_size> ',' StringLiteral ')'
        [Nonblockstm_Poke_Lparen_As_Comma_Rparen] = 42 ' <Non-Block Stm> ::= poke '(' <Expression> as <reg_size> ',' <Expression> ')'
        [Nonblockstm] = 43                        ' <Non-Block Stm> ::= <Variable> <Assign Op> <Expression>
        [Nonblockstm2] = 44                       ' <Non-Block Stm> ::= <sub_call>
        [Function_dec_s_Function_Id_As_End_Function] = 45 ' <function_dec_s> ::= <opt_inline> Function ID <Param List> as <var_type> <nl> <Statements> End Function
        [Sub_dec_s_Sub_Id_End_Sub] = 46           ' <sub_dec_s> ::= <opt_inline> Sub ID <Param List> <nl> <Statements> End Sub
        [Var_opt_param_Lbracket_Rbracket] = 47    ' <var_opt_param> ::= '[' <var_Argument_dec> ']'
        [Var_opt_param_Lbracket_Rbracket2] = 48   ' <var_opt_param> ::= '[' ']'
        [Var_opt_param] = 49                      ' <var_opt_param> ::= 
        [Var_opt_assing_Eq] = 50                  ' <var_opt_assing> ::= '=' <Expression>
        [Var_opt_assing] = 51                     ' <var_opt_assing> ::= 
        [While_end_End_While] = 52                ' <while_end> ::= End While
        [While_end_Wend] = 53                     ' <while_end> ::= wend
        [Assignop_Eq] = 54                        ' <Assign Op> ::= '='
        [Assignop_Timeseq] = 55                   ' <Assign Op> ::= '*='
        [Assignop_Diveq] = 56                     ' <Assign Op> ::= '/='
        [Assignop_Backslasheq] = 57               ' <Assign Op> ::= '\='
        [Assignop_Pluseq] = 58                    ' <Assign Op> ::= '+='
        [Assignop_Minuseq] = 59                   ' <Assign Op> ::= '-='
        [Assignop_Ltlteq] = 60                    ' <Assign Op> ::= '<<='
        [Assignop_Gtgteq] = 61                    ' <Assign Op> ::= '>>='
        [Assignop_Ampeq] = 62                     ' <Assign Op> ::= '&='
        [Assignop_Pipeeq] = 63                    ' <Assign Op> ::= '|='
        [Assignop_Careteq] = 64                   ' <Assign Op> ::= '^='
        [Assignop_Tildeeq] = 65                   ' <Assign Op> ::= '~='
        [Next_fn_Next] = 66                       ' <next_fn> ::= next
        [Next_fn_Next_Id] = 67                    ' <next_fn> ::= next ID
        [Dim_g_Dim] = 68                          ' <dim_g> ::= dim
        [Dim_g_Global] = 69                       ' <dim_g> ::= global
        [Opt_inline_Inline] = 70                  ' <opt_inline> ::= inline
        [Opt_inline] = 71                         ' <opt_inline> ::= 
        [Cond_comp_beg_If] = 72                   ' <cond_comp_beg> ::= if
        [Cond_comp_beg_Ifdef] = 73                ' <cond_comp_beg> ::= ifdef
        [Cond_comp_beg_Ifndef] = 74               ' <cond_comp_beg> ::= ifndef
        [Cond_comp_block_Numelse] = 75            ' <cond_comp_block> ::= '#Else' <cond_comp_beg> <Expression> <nl> <Statements> <cond_comp_block>
        [Cond_comp_block_Numelse2] = 76           ' <cond_comp_block> ::= '#Else' <nl> <Statements>
        [Cond_comp_block] = 77                    ' <cond_comp_block> ::= 
        [Ifstm_If_Then] = 78                      ' <If Stm> ::= if <Expression> Then <nl> <Statements> <If Blocks> <if_e> <nl>
        [Ifstm_If_Then2] = 79                     ' <If Stm> ::= if <Expression> Then <Non-Block Stm> <nl>
        [Ifstm_If_Then_Else] = 80                 ' <If Stm> ::= if <Expression> Then <Non-Block Stm> Else <Non-Block Stm> <nl>
        [Ifblocks_Then] = 81                      ' <If Blocks> ::= <else_if> <Expression> Then <nl> <Statements> <If Blocks>
        [Ifblocks_Else] = 82                      ' <If Blocks> ::= Else <nl> <Statements>
        [Ifblocks] = 83                           ' <If Blocks> ::= 
        [If_e_End_If] = 84                        ' <if_e> ::= End if
        [If_e_Endif] = 85                         ' <if_e> ::= endif
        [Else_if_Elseif] = 86                     ' <else_if> ::= Elseif
        [Else_if_Else_If] = 87                    ' <else_if> ::= Else if
        [Var_type_Byte] = 88                      ' <var_type> ::= <opt_var_attb> Byte
        [Var_type_Word] = 89                      ' <var_type> ::= <opt_var_attb> word
        [Var_type_Long] = 90                      ' <var_type> ::= <opt_var_attb> long
        [Var_type_Integer] = 91                   ' <var_type> ::= <opt_var_attb> integer
        [Var_type_String] = 92                    ' <var_type> ::= string
        [Var_type_Fixed] = 93                     ' <var_type> ::= <opt_var_attb> fixed
        [Var_type_Single] = 94                    ' <var_type> ::= <opt_var_attb> single
        [Var_type_New_Id] = 95                    ' <var_type> ::= new ID
        [Var_type_Id] = 96                        ' <var_type> ::= ID
        [Opt_var_attb_Signed] = 97                ' <opt_var_attb> ::= signed
        [Opt_var_attb_Unsigned] = 98              ' <opt_var_attb> ::= unsigned
        [Opt_var_attb] = 99                       ' <opt_var_attb> ::= 
        [Enumlist] = 100                          ' <Enum List> ::= <Enum Item> <Enum List>
        [Enumlist2] = 101                         ' <Enum List> ::= 
        [Enumitem_Dim_Id_As] = 102                ' <Enum Item> ::= dim ID as <var_type> <nl>
        [Enumitem] = 103                          ' <Enum Item> ::= <sub_dec_s> <nl>
        [Enumitem2] = 104                         ' <Enum Item> ::= <function_dec_s> <nl>
        [Selectstm_Select_End_Select] = 105       ' <Select Stm> ::= Select <Expression> <nl> <Select Blocks> End Select <nl>
        [Selectblocks_Case] = 106                 ' <Select Blocks> ::= Case <Expression> <nl> <Statements> <Select Blocks>
        [Selectblocks_Case_Else] = 107            ' <Select Blocks> ::= Case Else <nl> <Statements>
        [Selectblocks] = 108                      ' <Select Blocks> ::= 
        [Expression_Or] = 109                     ' <Expression> ::= <And Exp> Or <Expression>
        [Expression_Xor] = 110                    ' <Expression> ::= <And Exp> XOr <Expression>
        [Expression] = 111                        ' <Expression> ::= <And Exp>
        [Andexp_And] = 112                        ' <And Exp> ::= <Not Exp> And <And Exp>
        [Andexp] = 113                            ' <And Exp> ::= <Not Exp>
        [Notexp_Not] = 114                        ' <Not Exp> ::= NOT <Compare Exp>
        [Notexp] = 115                            ' <Not Exp> ::= <Compare Exp>
        [Compareexp] = 116                        ' <Compare Exp> ::= <Shift Exp> <Compare Op> <Compare Exp>
        [Compareexp2] = 117                       ' <Compare Exp> ::= <Shift Exp>
        [Shiftexp_Ltlt] = 118                     ' <Shift Exp> ::= <Add Exp> '<<' <Shift Exp>
        [Shiftexp_Gtgt] = 119                     ' <Shift Exp> ::= <Add Exp> '>>' <Shift Exp>
        [Shiftexp] = 120                          ' <Shift Exp> ::= <Add Exp>
        [Addexp_Plus] = 121                       ' <Add Exp> ::= <Modulus Exp> '+' <Add Exp>
        [Addexp_Minus] = 122                      ' <Add Exp> ::= <Modulus Exp> '-' <Add Exp>
        [Addexp] = 123                            ' <Add Exp> ::= <Modulus Exp>
        [Modulusexp_Mod] = 124                    ' <Modulus Exp> ::= <Int Div Exp> Mod <Modulus Exp>
        [Modulusexp] = 125                        ' <Modulus Exp> ::= <Int Div Exp>
        [Intdivexp_Backslash] = 126               ' <Int Div Exp> ::= <Mult Exp> '\' <Int Div Exp>
        [Intdivexp] = 127                         ' <Int Div Exp> ::= <Mult Exp>
        [Multexp_Times] = 128                     ' <Mult Exp> ::= <Negate Exp> '*' <Mult Exp>
        [Multexp_Div] = 129                       ' <Mult Exp> ::= <Negate Exp> '/' <Mult Exp>
        [Multexp] = 130                           ' <Mult Exp> ::= <Negate Exp>
        [Negateexp_Minus] = 131                   ' <Negate Exp> ::= '-' <Value>
        [Negateexp_Tilde] = 132                   ' <Negate Exp> ::= '~' <Value>
        [Negateexp] = 133                         ' <Negate Exp> ::= <Value>
        [Value_Lparen_Rparen] = 134               ' <Value> ::= '(' <Expression> ')'
        [Value_Decliteral] = 135                  ' <Value> ::= decLiteral
        [Value_Fixliteral] = 136                  ' <Value> ::= fixliteral
        [Value_Hexliteral] = 137                  ' <Value> ::= hexLiteral
        [Value_Binliteral] = 138                  ' <Value> ::= binLiteral
        [Value_Chrliteral] = 139                  ' <Value> ::= chrliteral
        [Value_Stringliteral] = 140               ' <Value> ::= StringLiteral
        [Value_True] = 141                        ' <Value> ::= True
        [Value_False] = 142                       ' <Value> ::= False
        [Value_Addressof_Lparen_Rparen] = 143     ' <Value> ::= AddressOf '(' <Variable> ')'
        [Value_Pop_Lparen_Stringliteral_As_Rparen] = 144 ' <Value> ::= pop '(' StringLiteral as <reg_size> ')'
        [Value_Peek_Lparen_As_Rparen] = 145       ' <Value> ::= peek '(' <Expression> as <reg_size> ')'
        [Value_Bit_test_Lparen_Comma_Rparen] = 146 ' <Value> ::= 'Bit_Test' '(' <Expression> ',' <Expression> ')'
        [Value_Id_Dot_Decliteral] = 147           ' <Value> ::= ID '.' decLiteral
        [Value] = 148                             ' <Value> ::= <Variable>
        [Value2] = 149                            ' <Value> ::= <func_call>
        [Value_Lbracket_Rbracket] = 150           ' <Value> ::= '[' <data list> ']'
        [Variable_Id_Lbracket_Rbracket] = 151     ' <Variable> ::= ID '[' <var_Argument> ']'
        [Variable_Id_Lbracket_Rbracket_Dot_Id] = 152 ' <Variable> ::= ID '[' <var_Argument> ']' '.' ID
        [Variable_Id_Dot_Id] = 153                ' <Variable> ::= ID '.' ID
        [Variable_Id] = 154                       ' <Variable> ::= ID
        [Func_call] = 155                         ' <func_call> ::= <Variable> <Argument List_F>
        [Sub_call] = 156                          ' <sub_call> ::= <Variable> <Argument List_F>
        [Compareop_Eq] = 157                      ' <Compare Op> ::= '='
        [Compareop_Ltgt] = 158                    ' <Compare Op> ::= '<>'
        [Compareop_Lt] = 159                      ' <Compare Op> ::= '<'
        [Compareop_Gt] = 160                      ' <Compare Op> ::= '>'
        [Compareop_Gteq] = 161                    ' <Compare Op> ::= '>='
        [Compareop_Lteq] = 162                    ' <Compare Op> ::= '<='
        [Datalist_Comma] = 163                    ' <data list> ::= <data> ',' <data list>
        [Datalist] = 164                          ' <data list> ::= <data>
        [Data_Decliteral] = 165                   ' <data> ::= decLiteral
        [Data_Fixliteral] = 166                   ' <data> ::= fixliteral
        [Data_Minus_Decliteral] = 167             ' <data> ::= '-' decLiteral
        [Data_Minus_Fixliteral] = 168             ' <data> ::= '-' fixliteral
        [Data_Hexliteral] = 169                   ' <data> ::= hexLiteral
        [Data_Binliteral] = 170                   ' <data> ::= binLiteral
        [Data_Stringliteral] = 171                ' <data> ::= StringLiteral
        [Data_Chrliteral] = 172                   ' <data> ::= chrliteral
        [Data_Id] = 173                           ' <data> ::= ID
        [Var_argument_dec_Comma] = 174            ' <var_Argument_dec> ::= <var_arg_dec> ',' <var_Argument_dec>
        [Var_argument_dec] = 175                  ' <var_Argument_dec> ::= <var_arg_dec>
        [Var_arg_dec_Decliteral] = 176            ' <var_arg_dec> ::= decLiteral
        [Var_arg_dec_Hexliteral] = 177            ' <var_arg_dec> ::= hexLiteral
        [Var_arg_dec_Binliteral] = 178            ' <var_arg_dec> ::= binLiteral
        [Var_arg_dec_Chrliteral] = 179            ' <var_arg_dec> ::= chrliteral
        [Var_argument_Comma] = 180                ' <var_Argument> ::= <var_arg> ',' <var_Argument>
        [Var_argument] = 181                      ' <var_Argument> ::= <var_arg>
        [Var_arg] = 182                           ' <var_arg> ::= <Expression>
        [Argumentlist_f_Lparen_Rparen] = 183      ' <Argument List_F> ::= '(' <Argument Items_F> ')'
        [Argumentlist_f_Lparen_Rparen2] = 184     ' <Argument List_F> ::= '(' ')'
        [Argumentitems_f_Comma] = 185             ' <Argument Items_F> ::= <Expression> ',' <Argument Items_F>
        [Argumentitems_f] = 186                   ' <Argument Items_F> ::= <Expression>
        [Paramlist_Lparen_Rparen] = 187           ' <Param List> ::= '(' <Param Items> ')'
        [Paramlist_Lparen_Rparen2] = 188          ' <Param List> ::= '(' ')'
        [Paramitems_Comma] = 189                  ' <Param Items> ::= <Param Item> ',' <Param Items>
        [Paramitems] = 190                        ' <Param Items> ::= <Param Item>
        [Paramitem_Byval_Id_As] = 191             ' <Param Item> ::= ByVal ID as <var_type>
        [Paramitem_Byref_Id_As] = 192             ' <Param Item> ::= ByRef ID as <var_type>
        [Reg_size_Byte] = 193                     ' <reg_size> ::= Byte
        [Reg_size_Word] = 194                     ' <reg_size> ::= word
        [Reg_size_Integer] = 195                  ' <reg_size> ::= integer
        [Reg_size_Long] = 196                     ' <reg_size> ::= long
    End Enum

    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    'valore Data Type
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    Public tipo_de_Dados_em_bytes() As Integer = {1, 1, 2, 2, 4, 4, 2, 2, 4, 4}
    Public tipo_de_Dados_str() As String = New String() {"ubyte", "sbyte", "uword", "sword", "ulong", "slong", "ufixed", "sfixed", "usingle", "ssingle"}
    Public identificadores_construcao_nome() As String = New String() {"xxx", "Variavel Global", "Variavel Local", "Funcao", "Subrotina", "Structure", "Structure", "Label"}

    Public Const Data_type_ubyte As Integer = 0
    Public Const Data_type_sbyte As Integer = 1
    Public Const Data_type_uword As Integer = 2
    Public Const Data_type_sword As Integer = 3
    Public Const Data_type_ulong As Integer = 4
    Public Const Data_type_slong As Integer = 5
    Public Const Data_type_ufixed As Integer = 6
    Public Const Data_type_sfixed As Integer = 7
    Public Const Data_type_usingle As Integer = 8
    Public Const Data_type_ssingle As Integer = 9
End Module
