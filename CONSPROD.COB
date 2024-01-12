       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSPROD.
      **************************************************
      * CONSULTA DE PRODUTO *
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT ARQPROD ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD
                    FILE STATUS  IS ST-ERRO.
					
       DATA DIVISION.
       FILE SECTION.
      *
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
       01 CCOD           PIC 9(06) VALUE ZEROS.
      * 01 CRSONOME        PIC X(30) VALUE SPACES
      * 01 CEMAIL          PIC X(23) VALUE SPACES.
       
       01 DET2.
          03 FILLER        PIC X(07) VALUE SPACES.
          03 DETCOD        PIC 9(06) VALUE ZEROS.
          03 FILLER        PIC X(07) VALUE SPACES.
          03 DETDESC       PIC X(25) VALUE SPACES.
          03 FILLER        PIC X(01) VALUE SPACES.
          03 DETQTD        PIC 9(06) VALUE ZEROS.
		  03 FILLER        PIC X(11) VALUE SPACES.
          03 DETPRECO      PIC 9(06)V99 VALUE ZEROS.
       
       
       SCREEN SECTION.
       
       01  TELACCLI.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                            CONSULTA DE".
           05  LINE 01  COLUMN 41 
               VALUE  "PRODUTOS".
           05  LINE 03  COLUMN 01 
               VALUE  "       CODIGO          DESCRICAO".
           05  LINE 03  COLUMN 41 
               VALUE  "    QUANTIDADE           PRECO".
           05  TCCOD
               LINE 05  COLUMN 08  PIC 9(06)
               USING  CCOD.
			   
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT ARQPROD
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE ARQPROD NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ARQPROD"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
       INC01.
           MOVE ZEROS TO CCOD
           DISPLAY TELACCLI.
       INC-001A.
           DISPLAY (22, 12)
            "F1 = ENCERRA CONSULTA"
           ACCEPT TCCOD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQPROD
                      GO TO ROT-FIM.
           IF W-ACT > 03
                      MOVE "FUNCAO NAO DEFINIDA" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CCOD TO COD
           START ARQPROD KEY IS NOT LESS COD INVALID KEY
                 MOVE "CHAVE NAO ENCONTRADA" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC01.
           GO TO INC-RD2.
       INC-RD2.
           DISPLAY (22, 12) LIMPA
           READ ARQPROD NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "FIM DO ARQUIVO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQPROD"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
           MOVE COD  TO DETCOD
           MOVE DESC TO DETDESC
           MOVE QTD TO DETQTD
		   MOVE PRECO TO DETPRECO
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
           CLOSE ARQPROD.
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