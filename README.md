# NEXTBasic_4_0

Esse projeto é referente ao compilador de linguagem Basic multplataforma.

O Parser foi escrito em BNF feito na Gold Parser Engine e o compilador esta sendo escrito em Visual Basic DotNet.

  O grammar esta disponivel na pasta: /NEXTBASIC_Grammar_4_0/NEXTBasic_Grammar_4_0.grm
  
  Toda vez que o Grammar for alterado é necessario gerar a nova tabela EGT (Enhanced Grammar Tables) e o novo esqueleto do modulo de analise recursiva, isso pode ser feito usando o Gold Parser Builder, eu recomendo a versão 5.2 que esta disponivel no site oficial do Gold Parser:
  
  http://www.goldparser.org/builder/index.htm
  
  É importante que as alterações no Grammar sejam feitas com muita cautela (e se possivel evitadas ao maximo a menos que sejam realmente necessarias) pois sempre que um novo esqueleto for gerado sera necessario migrar esse código para o projedo no Visual Studio o que inclui alterar 3 arquivos de código fonte (gmr_cabecalho.vb, NEXTBasic_Grammar_4_0.vb e Pre_Processor.vb).


#Lista de coisas a fazer:

  - Completar o modulo de analise recursiva do pre-processor
    -Percorrer a Arvore de Sintaxe Abstrata
    -Registrar os identificadores (Alias, Strucutures, Funções, Subrotinas, labels, variaveis Globais, includes, etc.)
    -Aplicar alterações necessarias no source code pré-compaliação
    
  - Completar o modulo de analise recursiva do Compilador
    -Percorrer a Arvore de Sintaxe Abstrata resolvendo os Steatments
    -A cada Steatmente resolvido gerar o algoritimo equivalente em Three-Code-Address
  
  - Completar o modulo de conversão do Código em Three-Code-address em Assembly para a plataforma desejada
  
  - Compilar o Assembly resultando da conversão (na etapa Anterior)
    
