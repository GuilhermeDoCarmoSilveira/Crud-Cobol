       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE002.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQCLI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS CPFCNPJ
                    FILE STATUS IS ST-ERRO.
       DATA DIVISION.
       FILE SECTION.
       FD ARQCLI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "REGCLI.DAT".
       01 REGCLI.
          03 CPFCNPJ        PIC 9(14).
          03 RSNOME         PIC X(60).
          03 APSOBRE        PIC X(60).
          03 CEP            PIC 9(08).
          03 LOGR           PIC X(40).
          03 NUM            PIC 9(08).
          03 COMPL          PIC X(15).
          03 BAIRRO         PIC X(22).
          03 CIDADE         PIC X(33).
          03 UF             PIC X(02).
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
       01 TPREGISTRO    PIC X(05) VALUE SPACES.
       01 DRSNOME       PIC X(13) VALUE SPACES.
       01 DAPSOBRE      PIC X(10) VALUE SPACES.
       01 DADO          PIC 9(14) VALUE ZEROS.
       01 CONT          PIC 9(02) VALUE ZEROS.
       

       SCREEN SECTION.
           
       01  TELACLI.
           05 BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "----------------------------CADASTRO DE".
           05  LINE 01  COLUMN 41 
               VALUE  "CLIENTES--------------------------------".
           05  LINE 08  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 08  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 09  COLUMN 01 
               VALUE  "                                 ENDERECO".
           05  LINE 09  COLUMN 41 
               VALUE  "O".
           05  LINE 11  COLUMN 01 
               VALUE  " CEP:               LOGRADOURO:".
           05  LINE 13  COLUMN 01 
               VALUE  " NUMERO:            COMPL:".
           05  LINE 13  COLUMN 41 
               VALUE  "    BAIRRO:".
           05  LINE 15  COLUMN 01 
               VALUE  " CIDADE:".
           05  LINE 15  COLUMN 41 
               VALUE  "    UF:".
           05  LINE 16  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 16  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 17  COLUMN 01 
               VALUE  " TELEFONE:".
           05  LINE 19  COLUMN 01 
               VALUE  " EMAIL:".
           05  LINE 20  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 20  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 21  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  LINE 22  COLUMN 01 
               VALUE  " OPCAO:".
           05  TTPREGISTO
               LINE 03  COLUMN 02  PIC X(05)
               USING  TPREGISTRO.
           05  TCPFCNPJ
               LINE 03  COLUMN 08  PIC 9(14)
               USING  CPFCNPJ.
           05  TDRSNOME
               LINE 05  COLUMN 02  PIC X(13)
               USING  DRSNOME.
           05  TRSNOME
               LINE 05  COLUMN 16  PIC X(60)
               USING  RSNOME.
           05  TDAPSOBRE
               LINE 07  COLUMN 02  PIC X(10)
               USING  DAPSOBRE.
           05  TAPSOBRE
               LINE 07  COLUMN 13  PIC X(60)
               USING  APSOBRE.
           05  TCEP
               LINE 11  COLUMN 07  PIC 99999/999
               USING  CEP.
           05  TLOGR
               LINE 11  COLUMN 33  PIC X(40)
               USING  LOGR.
           05  TNUM
               LINE 13  COLUMN 10  PIC 9(06)
               USING  NUM.
           05  TCOMPL
               LINE 13  COLUMN 28  PIC X(15)
               USING  COMPL.
           05  TBAIRRO
               LINE 13  COLUMN 53  PIC X(22)
               USING  BAIRRO.
           05  TCIDADE
               LINE 15  COLUMN 10  PIC X(33)
               USING  CIDADE.
           05  TUF
               LINE 15  COLUMN 49  PIC X(02)
               USING  UF.
           05  TTEL
               LINE 17  COLUMN 12  PIC 9(11)
               USING  TEL.
           05  TEMAIL
               LINE 19  COLUMN 09  PIC X(40)
               USING  EMAIL.
               
       PROCEDURE DIVISION.
       INICIO.
       R0.
       ABRE-ARQUIVO.
           OPEN I-O ARQCLI
           IF ST-ERRO IS NOT = "00"
           IF ST-ERRO = "30"
              OPEN OUTPUT ARQCLI
              MOVE "CRIANDO ARQUIVO CADASTRO DE CLIENTES" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              CLOSE ARQCLI
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
       R1.
           MOVE SPACES TO RSNOME APSOBRE LOGR COMPL BAIRRO 
           CIDADE UF EMAIL 
           MOVE ZEROS TO CPFCNPJ CEP NUM TEL W-SEL.
       RTELA.
           MOVE "PF|PJ" TO TPREGISTRO
           DISPLAY TELACLI.
       R2.
           DISPLAY(21, 12) "DIGITE O CPF OU CNPJ". 
       R2A.
           MOVE 0 TO CONT 
           MOVE ZEROS TO DADO
           DISPLAY TELACLI
           ACCEPT TCPFCNPJ
           MOVE CPFCNPJ TO DADO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              CLOSE ARQCLI
              STOP RUN.
           IF CPFCNPJ = ZEROS
              MOVE "CPF OU CNPJ VAZIO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2A.
       R2B.
           IF DADO > 0 
              DIVIDE 10 INTO DADO
              ADD 1 TO CONT
              GO TO R2B.
       R2C.
           IF CONT = 11
              MOVE "CPF: " TO TPREGISTRO
              MOVE "NOME INICIAL:" TO DRSNOME
              MOVE "SOBRENOME:" TO DAPSOBRE
           ELSE
              IF CONT = 14
                 MOVE "CNPJ:" TO TPREGISTRO
                 MOVE "RAZAO SOCIAL:" TO DRSNOME
                 MOVE "APELIDO  :" TO DAPSOBRE
              ELSE
                MOVE "CPF OU CNPJ INVALIDO, DIGITE NOVAMENTE" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R2A.
           DISPLAY TELACLI.	   
       LER-CAD.
           READ ARQCLI
           IF ST-ERRO NOT = "23"
              PERFORM R2C
              DISPLAY TELACLI
              MOVE "*** CLIENTE JA ESTA CADASTRADO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ACE-001
           ELSE
              NEXT SENTENCE.
       R3.
           ACCEPT TRSNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO RTELA.
           IF RSNOME = SPACES
              IF CONT = 11
                 MOVE "NOME EM BRANCO, DIGITE SEU PRIMEIRO NOME" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R3
              ELSE 
                 MOVE "RAZAO SOCIAL EM BRANCO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R3.
       R4.
           ACCEPT TAPSOBRE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.
           IF APSOBRE = SPACES
              IF CONT = 11
                 MOVE "NOME EM BRANCO, DIGITE SEU NOME COMPLETO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4
              ELSE 
                 MOVE "APELIDO EM BRANCO, INSIRA O APELIDO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4.    
       R5.
           ACCEPT TCEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R4.
           IF CEP = ZEROS
              MOVE "CEP VAZIO, DIGITE O CEP" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.
       R6.
           ACCEPT TNUM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R5.
           IF NUM = ZEROS
              MOVE "NUMERO VAZIO, DIGITE O NUMERO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
       R7.
           ACCEPT TCOMPL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R6.
           IF COMPL = SPACES
              MOVE "COMPLEMENTO VAZIO, DIGITE O COMPL" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7.
       R8.
           ACCEPT TTEL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R7.
           IF TEL = ZEROS
              MOVE "TELEFONE VAZIO, DIGITE O TELEFONE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.
       R9.
           ACCEPT TEMAIL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R8.
           IF EMAIL = SPACES
              MOVE "EMAIL VAZIO, DIGITE O EMAIL" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R9.
           IF W-SEL = 1 
           GO TO ALT-OPC.
       CONF.
           DISPLAY TELACLI.
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
           WRITE REGCLI.
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
           DISPLAY TELACLI.
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
           DELETE ARQCLI RECORD
           IF ST-ERRO = "00"
              MOVE "*** REGISTRO AMIGO EXCLUIDO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           MOVE "ERRO NA EXCLUSAO DO REGISTRO"   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.
       ALT-OPC.
           DISPLAY TELACLI.
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
           REWRITE REGCLI
           IF ST-ERRO = "00" OR "02"
              MOVE "*** REGISTRO ALTERADO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           MOVE "ERRO NA ALTERACAO DO REGISTRO AMIGO"   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.               
       ROT-FIM.
           CLOSE ARQCLI.
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
             
