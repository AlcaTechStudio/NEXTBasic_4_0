Module Three_Code_address

    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    ' Tabela de opcodes Three Code address
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    Public Const _comment As Integer = 0
    Public Const _ret As Integer = 1
    Public Const _retd As Integer = 2
    Public Const _dec_func As Integer = 3
    Public Const _label As Integer = 4
    Public Const _jump As Integer = 5
    Public Const _call As Integer = 6
    Public Const _lv_pointer As Integer = 7
    Public Const _stack_alloc As Integer = 8
    Public Const _stack_dealoc As Integer = 9
    Public Const _backup As Integer = 10
    Public Const _backdown As Integer = 11
    Public Const _setb As Integer = 12
    Public Const _clrb As Integer = 13
    Public Const _isbit_s As Integer = 14
    Public Const _dec_local_v As Integer = 15
    Public Const _dec_local_r As Integer = 16
    Public Const _dec_const As Integer = 17
    Public Const _dec_var As Integer = 18
    Public Const _copyext As Integer = 19
    Public Const _incasm As Integer = 20
    Public Const _copy As Integer = 21
    Public Const _copyesp As Integer = 22
    Public Const _jmpdfz As Integer = 23
    Public Const _isnot As Integer = 24
    Public Const _isdf As Integer = 25
    Public Const _iseq As Integer = 26
    Public Const _isleq As Integer = 27
    Public Const _isgeq As Integer = 28
    Public Const _islt As Integer = 29
    Public Const _isgt As Integer = 30
    Public Const _isleqs As Integer = 31
    Public Const _isgeqs As Integer = 32
    Public Const _islts As Integer = 33
    Public Const _isgts As Integer = 34
    Public Const _jmpeq As Integer = 35
    Public Const _neg As Integer = 36
    Public Const _inv As Integer = 37
    Public Const _add As Integer = 38
    Public Const _sub As Integer = 39
    Public Const _mult As Integer = 40
    Public Const _smult As Integer = 41
    Public Const _div As Integer = 42
    Public Const _sdiv As Integer = 43
    Public Const _mod As Integer = 44
    Public Const _sshiftl As Integer = 45
    Public Const _shiftl As Integer = 46
    Public Const _sshiftr As Integer = 47
    Public Const _shiftr As Integer = 48
    Public Const _and As Integer = 49
    Public Const _or As Integer = 50
    Public Const _xor As Integer = 51
    Public Const _scopyext As Integer = 52
    Public Const _incfile As Integer = 53
    Public Const _constdec As Integer = 54

    'Listas que Armazenam a Meta Linguagem
    Public meta_op As New List(Of Integer)
    Public meta_s As New List(Of String)
    Public meta_p1 As New List(Of String)
    Public meta_p2 As New List(Of String)
    Public meta_re As New List(Of String)

    ' Adiciona Instrucao a meta linguagem
    Public Sub adc_meta(ByVal op As Integer, ByVal s As String, ByVal p1 As String, ByVal p2 As String, ByVal re As String)
        meta_op.Add(op)
        meta_s.Add(s)
        meta_p1.Add(p1)
        meta_p2.Add(p2)
        meta_re.Add(re)
    End Sub
    ' Limpa listas que armazenam meta linguagem
    Public Sub clr_meta()
        meta_op.Clear()
        meta_s.Clear()
        meta_p1.Clear()
        meta_p2.Clear()
        meta_re.Clear()
    End Sub




End Module
