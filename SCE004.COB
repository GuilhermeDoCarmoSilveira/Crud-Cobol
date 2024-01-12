       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE004.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQPROD ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS COD
                    FILE STATUS IS ST-ERRO.
					
           SELECT ARQFOR ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS FOR-CPFCNPJ
                    FILE STATUS  IS ST-ERRO.
       DATA DIVISION.
       FILE SECTION.
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
		  03 PROD-CPFCNPJ	PIC 9(14).
          
       FD ARQFOR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "REGFOR.DAT".
       01 REGFOR.
          03 FOR-CPFCNPJ       PIC 9(14).
          03 FOR-RSNOME        PIC X(60).
          03 FOR-APNOME        PIC X(60).
		  03 FOR-CEP            PIC 9(08).
          03 FOR-NUM            PIC 9(08).
          03 FOR-COMPL          PIC X(15).
          03 FOR-TEL            PIC 9(11).
          03 FOR-EMAIL          PIC X(40).
          
       WORKING-STORAGE SECTION.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 W-CONT        PIC 9(06) VALUE ZEROS. 
       77 W-SEL         PIC 9(01) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 ST-ERRO       PIC X(02) VALUE "00".
       77 MENS          PIC X(67) VALUE SPACES.
       77 DADOS         PIC X(01) VALUE SPACES.
       77 DADO          PIC 9(14) VALUE ZEROS.
       77 CONT          PIC 9(02) VALUE ZEROS.
       77 DTPPROD       PIC X(15) VALUE SPACES.
       77 DUNIDADE      PIC X(10) VALUE SPACES.
	   77 RSNOME        PIC X(60) VALUE SPACES.
	   77 APSOBRE       PIC X(60) VALUE SPACES.
	   
	   
       01 TABTPPROD.
           03 VTTPPROD PIC X(15) OCCURS 9 TIMES.
           
       01 TXUNIDADE.
           03 UNIT-MAIU PIC X(02) VALUE SPACES.
           03 UNIT-MINU PIC X(02) VALUE SPACES.
           03 DUNIDADE PIC X(10) VALUE SPACES.
           
       01 TBUNIDADE.
           03 FILLER        PIC X(14) VALUE "PCpcPECA".
           03 FILLER        PIC X(14) VALUE "KGkgKILO".
           03 FILLER        PIC X(14) VALUE "LTltLITRO".
           03 FILLER        PIC X(14) VALUE "TNtnTONELADA".
           03 FILLER        PIC X(14) VALUE "LBlbLIBRA".
           03 FILLER        PIC X(14) VALUE "OZozONCA".
           03 FILLER        PIC X(14) VALUE "MGmgMILIGRAMA".
           
       01 TBUNIDADER REDEFINES TBUNIDADE.
           03 TABUNIDADE     PIC X(14) OCCURS 7 TIMES.
       01 IND                PIC 9(01) VALUE ZEROS.        
       
       SCREEN SECTION.
       
       01  TELAPRO.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "---------------------------CADASTRO DE P".
           05  LINE 01  COLUMN 41 
               VALUE  "RODUTOS---------------------------------".
           05  LINE 03  COLUMN 01 
               VALUE  " CODIGO:".
           05  LINE 05  COLUMN 01 
               VALUE  " DESCRICAO:".
           05  LINE 07  COLUMN 01 
               VALUE  " TIPO:                     UNIDADE:".
           05  LINE 09  COLUMN 01 
               VALUE  " ULT. PRECO: R$".
           05  LINE 11  COLUMN 01 
               VALUE  " DATA ULTIMA COMPRA:".
           05  LINE 13  COLUMN 01 
               VALUE  " QTD. EM ESTOQUE:".
           05  LINE 14  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 14  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 15  COLUMN 01 
               VALUE  " CPF/CNPJ FORNECEDOR:".
           05  LINE 17  COLUMN 01 
               VALUE  " NOME/RZ SOCIAL:".
           05  LINE 19  COLUMN 01 
               VALUE  " SBN/APELIDO:".
           05  LINE 20  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 20  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 21  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  LINE 22  COLUMN 01 
               VALUE  " OPCAO:".
           05  TCOD
               LINE 03  COLUMN 10  PIC 9(06)
               USING  COD.
           05  TDESC
               LINE 05  COLUMN 13  PIC X(30)
               USING  DESC.
           05  TTPPROD
               LINE 07  COLUMN 08  PIC 9(01)
               USING  TPPROD.
           05  TDTPPROD
               LINE 07  COLUMN 10  PIC X(15)
               USING  DTPPROD.
           05  TUNIDADE
               LINE 07  COLUMN 37  PIC X(02)
               USING  UNIDADE.
           05  TDUNIDADE
               LINE 07  COLUMN 40  PIC X(10)
               USING  DUNIDADE.
           05  TPRECO
               LINE 09  COLUMN 17  PIC 9(06)V99
               USING  PRECO.
           05  TDATACP
               LINE 11  COLUMN 22  PIC 99/99/9999
               USING  DATACP.
           05  TQTD
               LINE 13  COLUMN 19  PIC 9(06)
               USING  QTD.
           05  TCPFCNPJ
               LINE 15  COLUMN 23  PIC 9(14)
               USING  PROD-CPFCNPJ.
           05  TRSNOME
               LINE 17  COLUMN 18  PIC X(60)
               USING  RSNOME.
           05  TAPSOBRE
               LINE 19  COLUMN 15  PIC X(60)
               USING  APSOBRE.
               
      *
       01  TTIPOP.
           05  LINE 04  COLUMN 41 
               VALUE  "        1-SAUDE".
           05  LINE 05  COLUMN 41 
               VALUE  "        2-ALIMENTO".
           05  LINE 06  COLUMN 41 
               VALUE  "        3-VESTUARIO".
           05  LINE 07  COLUMN 41 
               VALUE  "        4-AGROPECUARIO".
           05  LINE 08  COLUMN 41 
               VALUE  "        5-PET".
           05  LINE 09  COLUMN 41 
               VALUE  "        6-BELEZA".
           05  LINE 10  COLUMN 41
               VALUE  "        7-ESPORTE".
           05  LINE 11  COLUMN 41
               VALUE  "        8-ELETRONICO".
           05  LINE 12  COLUMN 41
               VALUE  "        9-AUTOMOTIVO".
      * 
       01  TELAU.
           05  LINE 04  COLUMN 41 
               VALUE  "        PC - PECA".
           05  LINE 05  COLUMN 41 
               VALUE  "        KG - KILO".
           05  LINE 06  COLUMN 41 
               VALUE  "        LT - LITRO".
           05  LINE 07  COLUMN 41 
               VALUE  "        TN - TONELADA".
           05  LINE 08  COLUMN 41 
               VALUE  "        LB - LIBRA".
           05  LINE 09  COLUMN 41 
               VALUE  "        OZ - ONCA".
           05  LINE 10  COLUMN 41
               VALUE  "        MG - MILIGRAMA".
               
       PROCEDURE DIVISION.
       INICIO.
           MOVE "SAUDE"         TO VTTPPROD(1)
           MOVE "ALIMENTO"      TO VTTPPROD(2)
           MOVE "VESTUARIO"     TO VTTPPROD(3)
           MOVE "AGROPECUARIO"  TO VTTPPROD(4)
           MOVE "PET"           TO VTTPPROD(5)
           MOVE "BELEZA"        TO VTTPPROD(6)
           MOVE "ESPORTE"       TO VTTPPROD(7)
           MOVE "ELETRONICO"    TO VTTPPROD(8)
           MOVE "AUTOMOTIVO"    TO VTTPPROD(9).
       R0.
       ABRE-ARQUIVO.
           OPEN I-O ARQPROD
           IF ST-ERRO IS NOT = "00"
           IF ST-ERRO = "30"
              OPEN OUTPUT ARQPROD
              MOVE "CRIANDO ARQUIVO CADASTRO DE PRODUTOS" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              CLOSE ARQPROD
              GO TO R0
           ELSE
              IF ST-ERRO = "95"
                 MOVE "ISAM NAO CARREGADO " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADASTRO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.
       ABRE-FOR.
           OPEN INPUT ARQFOR
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE FORN. NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE PRODUTOS " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       R1.
           MOVE SPACES TO DESC UNIDADE FOR-RSNOME FOR-APNOME
		   FOR-COMPL FOR-EMAIL
           MOVE ZEROS TO COD TPPROD PRECO QTD FOR-CPFCNPJ 
           W-SEL DATACP FOR-CEP FOR-NUM FOR-TEL PROD-CPFCNPJ.
       RTELA.
           DISPLAY TELAPRO.
       R2.
           DISPLAY(21, 12) "DIGITE O CODIGO DO PRODUTO".
           ACCEPT TCOD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              CLOSE ARQPROD ARQFOR
              STOP RUN.
           IF COD = ZEROS 
              MOVE "CODIGO EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.
       LER-CAD.
           READ ARQPROD
           IF ST-ERRO NOT = "23"
              PERFORM R4A
              PERFORM R5B
              PERFORM R9A
              DISPLAY TELAPRO
              MOVE "*** PRODUTO JA ESTA CADASTRADO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ACE-001
           ELSE
              NEXT SENTENCE.
       R3.
           ACCEPT TDESC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R2
           IF DESC = SPACES 
              MOVE "DESCRICAO EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.
       R4.
           DISPLAY TTIPOP
           ACCEPT TTPPROD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.
           IF TPPROD = SPACES
              MOVE "DIGITE UM NUMERO DE ACORDO COM A TABELA" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.
           IF TPPROD > 9
              MOVE "DIGITE UM NUMERO DE ACORDO COM A TABELA" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.
       R4A.
           MOVE VTTPPROD(TPPROD) TO DTPPROD
           DISPLAY TELAPRO.
       R5.
           DISPLAY TELAU
           ACCEPT TUNIDADE
		   ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R4.
       R5A.
           MOVE 1 TO IND.
       R5B.
           MOVE TABUNIDADE (IND) TO TXUNIDADE
           IF UNIDADE = UNIT-MAIU OR UNIDADE = UNIT-MINU
              DISPLAY TDUNIDADE
           ELSE
              ADD 1 TO IND
              IF IND > 7
                 MOVE "UNIDADE NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5
              ELSE
                 GO TO R5B.
       R6.
           DISPLAY TELAPRO
           ACCEPT TPRECO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R5
           IF PRECO = ZEROS 
              MOVE "PRECO VAZIO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
       R7.
           ACCEPT TDATACP
           ACCEPT W-ACT FROM ESCAPE KEY 
           IF W-ACT = 01
              GO TO R6.
           IF DATACP = SPACES
              MOVE "DATA EM BRANCO, DIGITE UMA DATA" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7.
       R8.
           ACCEPT TQTD
           ACCEPT W-ACT FROM ESCAPE KEY 
           IF W-ACT = 01
              GO TO R7.
       R9.
           ACCEPT TCPFCNPJ
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R8.
           IF PROD-CPFCNPJ = ZEROS
              MOVE "CPF VAZIO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R9.
       R9A.
           MOVE PROD-CPFCNPJ TO FOR-CPFCNPJ
           READ ARQFOR
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** CPF/CNPJ NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                MOVE FOR-RSNOME TO RSNOME
                MOVE FOR-APNOME TO APSOBRE
                DISPLAY TELAPRO.
           IF W-SEL = 1
              GO TO ALT-OPC.
       CONF.
           DISPLAY TELAPRO.
           DISPLAY (21, 12) "QUER SALVAR OS DADOS? (S/N)"
           ACCEPT (22, 9) DADOS
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
           WRITE REGPROD.
           GO TO R1.
       ACE-001.
           DISPLAY (21, 12) "N = NOVO REGISTRO | A = ALTERAR |" 
           " E = EXCLUIR | S = SAIR"  
           ACCEPT (22, 9) W-OPCAO
           IF W-OPCAO = "N" OR "n"
              GO TO R1.
           IF W-OPCAO = "A" OR "a" 
              MOVE 1 TO W-SEL
              GO TO R3.
           IF W-OPCAO = "S" OR "s"
              STOP RUN.
           IF W-OPCAO NOT = "E" AND "e"
              GO TO ACE-001.
       EXC-OPC.
           DISPLAY TELAPRO.
           DISPLAY (21, 12) "EXCLUIR   (S/N) : ".
           ACCEPT (22, 9) W-OPCAO
           IF W-OPCAO = "N" OR "n"
              MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO EXC-OPC.
        EXC-DL1.
           DELETE ARQPROD RECORD
           IF ST-ERRO = "00"
              MOVE "*** REGISTRO PRODUTO EXCLUIDO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           MOVE "ERRO NA EXCLUSAO DO REGISTRO"   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.
        ALT-OPC.
           DISPLAY TELAPRO.
           DISPLAY (21, 12) "ALTERAR (S/N) : ".
           ACCEPT (22, 9) W-OPCAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO R9.
           IF W-OPCAO = "N" OR "n"
              MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "DIGITE APENAS S = SIM e N = NAO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ALT-OPC.
       ALT-RW1.
           REWRITE REGPROD
           IF ST-ERRO = "00" OR "02"
              MOVE "*** REGISTRO ALTERADO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           MOVE "ERRO NA ALTERACAO DO REGISTRO AMIGO"   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.               
       ROT-FIM.
           CLOSE ARQPROD ARQFOR.
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (21, 12) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
              GO TO ROT-MENS2
           ELSE
              MOVE SPACES TO MENS
              DISPLAY (21, 12) MENS.
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