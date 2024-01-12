       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSCLI.
      **************************************************
      * CONSULTA DE CLIENTE *
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT CADCLI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPFCNPJ
                    FILE STATUS  IS ST-ERRO.
					
       DATA DIVISION.
       FILE SECTION.
      *
       FD CADCLI
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
       01 W-SEL             PIC 9(01) VALUE ZEROS.
       01 W-CONT         PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO        PIC X(02) VALUE "00".
       01 W-ACT            PIC 9(02) VALUE ZEROS.
       01 MENS             PIC X(50) VALUE SPACES.
       01 LIMPA             PIC X(55) VALUE SPACES.
       01 SOLIC             PIC X(20) VALUE SPACES.
       01 CONLIN          PIC 9(03) VALUE 001.
       01 CCPFCNPJ        PIC 9(14) VALUE ZEROS.
      * 01 CRSONOME        PIC X(30) VALUE SPACES
      * 01 CEMAIL          PIC X(23) VALUE SPACES.
       
       01 DET2.
          03 FILLER        PIC X(01) VALUE SPACES.
          03 DETCPFCNPJ    PIC 9(14) VALUE ZEROS.
          03 FILLER        PIC X(04) VALUE SPACES.
          03 DETRSNOME     PIC X(30) VALUE SPACES.
          03 FILLER        PIC X(06) VALUE SPACES.
          03 DETEMAIL      PIC X(23) VALUE SPACES.
       
       SCREEN SECTION.
       
       01  TELACCLI.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                            CONSULTA DE".
           05  LINE 01  COLUMN 41 
               VALUE  "CLIENTES".
           05  LINE 03  COLUMN 01 
               VALUE  "    CNPJ/CPF             RAZAO SOCIAL /".
           05  LINE 03  COLUMN 41 
               VALUE  "NOME                  EMAIL".
           05  TCCPFCNPJ
               LINE 05  COLUMN 02  PIC 9(14)
               USING  CCPFCNPJ.

       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT CADCLI
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADCLI NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADCLI"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
       INC01.
           MOVE ZEROS TO CCPFCNPJ
           DISPLAY TELACCLI.
       INC-001A.
           DISPLAY (22, 12)
            "F1 = ENCERRA CONSULTA"
           ACCEPT TCCPFCNPJ
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADCLI
                      GO TO ROT-FIM.
           IF W-ACT > 03
                      MOVE "FUNCAO NAO DEFINIDA" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CCPFCNPJ TO CPFCNPJ
           START CADCLI KEY IS NOT LESS CPFCNPJ INVALID KEY
                 MOVE "CHAVE NAO ENCONTRADA" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC01.
           GO TO INC-RD2.
       INC-RD2.
           DISPLAY (22, 12) LIMPA
           READ CADCLI NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "FIM DO ARQUIVO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCLI"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
           MOVE CPFCNPJ TO DETCPFCNPJ
           MOVE RSNOME TO DETRSNOME
           MOVE EMAIL TO DETEMAIL
           COMPUTE LIN = CONLIN + 4
           DISPLAY (LIN, 01) DET2
           ADD 1 TO CONLIN
           IF CONLIN < 17
                  GO TO INC-RD2.
       ROT-SOL.
           DISPLAY (23, 12) "CONTINUA CONSULTA :"
           MOVE "S" TO W-OPCAO
		   DISPLAY (24, 12) "OPCAO: "
           ACCEPT  (24, 19) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TELACCLI
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "DIGITE S = SIM | N = NAO" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           GO TO INC01.
       NOV-SOL.
           DISPLAY (23, 12) "NOVA CONSULTA :"
           MOVE "S" TO W-OPCAO
		   DISPLAY (24, 12) "OPCAO: "
           ACCEPT  (24, 19) W-OPCAO WITH UPDATE
           IF W-OPCAO = "S" OR "s"
                  MOVE 01 TO CONLIN
                  GO TO INC01.
           IF W-OPCAO = "N" OR "n"
                  GO TO ROT-FIM
		   ELSE
                  MOVE "DIGITE S = SIM   N = NAO" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE CADCLI.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
           
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (22, 12) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (22, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.