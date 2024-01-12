       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSPED.
      **************************************************
      * CONSULTA DE PEDIDO *
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT ARQPED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CHAVE
                    FILE STATUS  IS ST-ERRO.
					
       DATA DIVISION.
       FILE SECTION.
      *
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
		  
       WORKING-STORAGE SECTION.
       01 W-SEL          PIC 9(01) VALUE ZEROS.
       01 W-CONT         PIC 9(04) VALUE ZEROS.
       01 W-OPCAO        PIC X(01) VALUE SPACES.
       01 ST-ERRO        PIC X(02) VALUE "00".
       01 W-ACT          PIC 9(02) VALUE ZEROS.
       01 MENS           PIC X(50) VALUE SPACES.
       01 LIMPA          PIC X(55) VALUE SPACES.
       01 SOLIC          PIC X(20) VALUE SPACES.
       01 CONLIN         PIC 9(03) VALUE 001.
	   01 CCHAVE         PIC 9(07) VALUE ZEROS.
        
       01 DET2.
          03 FILLER        PIC X(06) VALUE SPACES.
          03 DETCHAVE      PIC 9(10) VALUE ZEROS.
		  03 FILLER        PIC X(08) VALUE SPACES.
          03 DETCODPROD    PIC X(06) VALUE SPACES.
          03 FILLER        PIC X(14) VALUE SPACES.
          03 DETQTD        PIC 9(06) VALUE ZEROS.
		  03 FILLER        PIC X(12) VALUE SPACES.
          03 DETPRECOT     PIC 9(06)V99 VALUE ZEROS.
       
       SCREEN SECTION.
       
       01  TELACCLI.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                           CONSULTA DE P".
           05  LINE 01  COLUMN 41 
               VALUE  "EDIDOS".
           05  LINE 03  COLUMN 01 
               VALUE  "       CHAVE         COD. PRODUTO".
           05  LINE 03  COLUMN 41 
               VALUE  "  QUANTIDADE         PRECO TOTAL".
           05  TCCHAVE
               LINE 05  COLUMN 07  PIC 9(07)
               USING  CCHAVE.
	   
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT ARQPED
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE ARQPED NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ARQPED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
       INC01.
           MOVE ZEROS TO CCHAVE
           DISPLAY TELACCLI.
       INC-001A.
           DISPLAY (22, 12)
            "F1 = ENCERRA CONSULTA"
           ACCEPT TCCHAVE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQPED
                      GO TO ROT-FIM.
           IF W-ACT > 03
                      MOVE "FUNCAO NAO DEFINIDA" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CCHAVE TO CHAVE
           START ARQPED KEY IS NOT LESS CHAVE INVALID KEY
                 MOVE "CHAVE NAO ENCONTRADA" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC01.
           GO TO INC-RD2.
       INC-RD2.
           DISPLAY (22, 12) LIMPA
           READ ARQPED NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "FIM DO ARQUIVO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQPED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
           MOVE CHAVE TO DETCHAVE
           MOVE CODPROD TO DETCODPROD
		   MOVE PED-QTD TO DETQTD
		   MOVE TOTAL TO DETPRECOT
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
           CLOSE ARQPED.
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