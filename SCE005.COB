       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE005.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQPED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS CHAVE
                    FILE STATUS IS ST-ERRO.

           SELECT ARQPROD ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS COD
                    FILE STATUS IS ST-ERRO.
                    
            SELECT ARQCLI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS CPFCNPJ
                    FILE STATUS IS ST-ERRO.     
       DATA DIVISION.
       FILE SECTION.
       FD ARQPED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "REGPED.DAT".
       01 REGPED.
          03 CHAVE.
           05 NUMPEDIDO         PIC 9(06).
           05 TPPEDIDO          PIC 9(01).
          03 CODPROD            PIC 9(06).
          03 CODCLI             PIC 9(14).
          03 CODFORN            PIC 9(14).
          03 PED-QTD            PIC 9(06).
          03 PRECOU             PIC 9(06)V99.
          03 TOTAL              PIC 9(05)V99.
       
       FD ARQPROD
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "REGPROD.DAT".
       01 REGPROD.
          03 COD            PIC 9(06).
          03 DESC           PIC X(30).
          03 UNIDADE        PIC X(02).
          03 TPPROD         PIC 9(01).
          03 PRECO          PIC 9(06)V99.
          03 DATACP         PIC 9(08).
          03 QTD            PIC 9(06).
          03 PROD-CPFCNPJ   PIC 9(14).
          
        FD ARQCLI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "REGCLI.DAT".
       01 REGCLI.
          03 CPFCNPJ        PIC 9(14).
          03 RSNOME         PIC X(60).
          03 APSOBRE        PIC X(60).
          03 CEP            PIC 9(08).
          03 NUM            PIC 9(08).
          03 COMPL          PIC X(15).
          03 TEL            PIC 9(11).
          03 EMAIL          PIC X(40).
          
       WORKING-STORAGE SECTION.
       
       01 W-ACT         PIC 9(02) VALUE ZEROS.
       01 W-CONT        PIC 9(06) VALUE ZEROS. 
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 MENS          PIC X(67) VALUE SPACES.
       01 DADOS         PIC X(01) VALUE SPACES.
       01 DADO          PIC 9(14) VALUE ZEROS.
       01 CONT          PIC 9(02) VALUE ZEROS.
	   01 DTPPED		PIC X(06) VALUE SPACES.
	   01 DESCPROD		PIC X(30) VALUE SPACES.
       
       SCREEN SECTION.
       
       01  TELAPED.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                                  PEDIDO".
           05  LINE 02  COLUMN 41 
               VALUE  "S".
           05  LINE 05  COLUMN 01 
               VALUE  " NUM PEDIDO:         TIPO:".
           05  LINE 07  COLUMN 01 
               VALUE  " COD. PRODUTO:         NOME DO PRODUTO:".
           05  LINE 09  COLUMN 01 
               VALUE  " COD. CLIENTE:".
           05  LINE 11  COLUMN 01 
               VALUE  " COD. FORNECEDOR:".
           05  LINE 13  COLUMN 01 
               VALUE  " QUANTIDADE:".
           05  LINE 15  COLUMN 01 
               VALUE  " PRECO UNITARIO:".
           05  LINE 17  COLUMN 01 
               VALUE  " VALOR TOTAL:".
           05  LINE 20  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  LINE 21  COLUMN 01 
               VALUE  " OPCAO:".
           05  TNUMPEDIDO
               LINE 05  COLUMN 14  PIC 9(06)
               USING  NUMPEDIDO.
           05  TTPPEDIDO
               LINE 05  COLUMN 28  PIC 9(01)
               USING  TPPEDIDO.
           05  TDTPPED
               LINE 05  COLUMN 31  PIC X(06)
               USING  DTPPED.
           05  TCODPROD
               LINE 07  COLUMN 16  PIC 9(06)
               USING  CODPROD.
           05  TDESCPROD
               LINE 07  COLUMN 41  PIC X(30)
               USING  DESCPROD.
           05  TCODCLI
               LINE 09  COLUMN 16  PIC 9(14)
               USING  CODCLI.
           05  TCODFORN
               LINE 11  COLUMN 19  PIC 9(14)
               USING  CODFORN.
           05  TQTD
               LINE 13  COLUMN 14  PIC 9(06)
               USING  PED-QTD.
           05  TPRECOU
               LINE 15  COLUMN 18  PIC 9(06)
               USING  PRECOU.
           05  TTOTAL
               LINE 17  COLUMN 15  PIC 9(08)
               USING  TOTAL.
               
       PROCEDURE DIVISION.
       INICIO.
       R0.
       ABRE-ARQUIVO.
           OPEN I-O ARQPED
           IF ST-ERRO IS NOT = "00"
           IF ST-ERRO = "30"
              OPEN OUTPUT ARQPED
              MOVE "CRIANDO ARQUIVO CADASTRO DE PEDIDOS" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              CLOSE ARQPED
              GO TO R0
           ELSE
              IF ST-ERRO = "95"
                 MOVE "ISAM NAO CARREGADO " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO PEDIDO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.
                 
       ABRE-PRODUTO.
           OPEN INPUT ARQPROD
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE PRODUTO NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE PRODUTO " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
                 
       ABRE-CLIENTE.
           OPEN INPUT ARQCLI
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CLIENTE NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CLIENTE " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       R1.
           MOVE SPACES TO DESC
           MOVE ZEROS TO PROD-CPFCNPJ CPFCNPJ W-SEL
           NUMPEDIDO TPPEDIDO CODPROD CODCLI CODFORN
           QTD PRECO TOTAL PROD-CPFCNPJ PRECOU PED-QTD.
       RTELA.
           DISPLAY TELAPED.
       R2.
           ACCEPT TNUMPEDIDO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              CLOSE ARQPED
              STOP RUN.
           IF NUMPEDIDO = ZEROS 
              MOVE "CODIGO EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.
       R3.
           DISPLAY(05, 31) "1 - COMPRA | 2 - VENDA".
		   ACCEPT TTPPEDIDO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO R2.
           IF TPPEDIDO = ZEROS 
              MOVE "TIPO DE PEDIDO EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.
           IF TPPEDIDO > 2
              MOVE "TIPO DE PEDIDO INEXISTENTE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.
       R3A.
           IF TPPEDIDO = 1
              MOVE "COMPRA" TO DTPPED
           ELSE
              MOVE "VENDA" TO DTPPED.
           DISPLAY TELAPED.			  
       LER-CAD.
           READ ARQPED
           IF ST-ERRO NOT = "23"
              PERFORM R3A
              PERFORM R4A
              PERFORM R5A  
              DISPLAY TELAPED
              MOVE "*** PEDIDO JA ESTA CADASTRADO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ACE-001
           ELSE
              NEXT SENTENCE.
       R4.
           ACCEPT TCODPROD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.
           IF CODPROD = ZEROS
              MOVE "CODIGO DO PRODUTO VAZIO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.
       R4A.
           MOVE CODPROD TO COD
           READ ARQPROD
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** PRODUTO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R4
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO DE PRODUTO" 
                  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
              MOVE DESC TO DESCPROD
              MOVE PRECO TO PRECOU
              MOVE PROD-CPFCNPJ TO CODFORN
              DISPLAY TELAPED.
       R5.
           IF TPPEDIDO = 1
              GO TO R6
           ELSE        
              ACCEPT TCODCLI.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
            GO TO R4.			  
           IF CODCLI = ZEROS
              MOVE "COD CLIENTE VAZIO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5. 
       R5A.
           MOVE CODCLI TO CPFCNPJ
           READ ARQCLI
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** CPF/CNPJ NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R5
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO DE CLIENTE" 
				  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELAPED.
       R6.
           ACCEPT TQTD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
            GO TO R5.             
           IF PED-QTD = ZEROS 
              MOVE "QUANTIDADE NULA" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
       R7.
           MULTIPLY PRECOU BY PED-QTD GIVING TOTAL
           DISPLAY TELAPED.
           IF W-SEL = 1
              GO TO ALT-OPC.
       CONF.
           DISPLAY TELAPED.
           DISPLAY (20, 12) "QUER SALVAR OS DADOS? (S/N)"
           ACCEPT (21, 9) DADOS
           IF DADOS = "S" OR "s"
              MOVE "DADOS FORAM SALVOS" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INC-WR1
           ELSE
              IF DADOS = "N" OR "n" 
                 MOVE "DADOS APAGADOS E NAO SALVOS" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R1
              ELSE
                 MOVE "DIGITE UMA OPERACAO VALIDA (S/N)" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO CONF.
       INC-WR1. 
           WRITE REGPED.
           GO TO R1.
       ACE-001.
           DISPLAY (20, 12) "N = NOVO REGISTRO | A = ALTERAR |" 
           " E = EXCLUIR | S = SAIR"  
           ACCEPT (21, 9) W-OPCAO
           IF W-OPCAO = "N" OR "n"
              GO TO R1.
           IF W-OPCAO = "A" OR "a" 
              MOVE 1 TO W-SEL
              GO TO R4.
           IF W-OPCAO = "S" OR "s"
              STOP RUN.
           IF W-OPCAO NOT = "E" AND "e"
              GO TO ACE-001.
       EXC-OPC.
           DISPLAY TELAPED.
           DISPLAY (20, 12) "EXCLUIR   (S/N) : ".
           ACCEPT (21, 9) W-OPCAO
           IF W-OPCAO = "N" OR "n"
              MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO EXC-OPC.
       EXC-DL1.
           DELETE ARQPED RECORD
           IF ST-ERRO = "00"
              MOVE "*** REGISTRO PEDIDO EXCLUIDO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           MOVE "ERRO NA EXCLUSAO DO REGISTRO"   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.
       ALT-OPC.
           DISPLAY TELAPED.
           DISPLAY (20, 12) "ALTERAR (S/N) : ".
           ACCEPT (21, 9) W-OPCAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO R6.
           IF W-OPCAO = "N" OR "n"
              MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "DIGITE APENAS S = SIM e N = NAO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ALT-OPC.
       ALT-RW1.
           REWRITE REGPED
           IF ST-ERRO = "00" OR "02"
              MOVE "*** REGISTRO ALTERADO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           MOVE "ERRO NA ALTERACAO DO REGISTRO PEDIDO"   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.               
       ROT-FIM.
           CLOSE ARQPED ARQPROD ARQCLI.
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (20, 12) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
              GO TO ROT-MENS2
           ELSE
              MOVE SPACES TO MENS
              DISPLAY (20, 12) MENS.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-TEMPO.
      
      *    FILE STATUS
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
      *    95 = ISAM NAO CARREGADO
      *    10 = FIM DA LEITURA ARQUIVO SEQUENCIAL
           
           