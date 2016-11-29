      *----------------------------------------------------------------*
      * PROGRAMA..: ARCSB628.
      * ANALISTA..: DIEGO PAZ CASAGRANDE.
      * AUTOR.....: DIEGO PAZ CASAGRANDE.
      * DATA......: 07/11/2016
      * OBJETIVO..: Motor PCLD - Combinação de grupos de operações e
      *             faixas de redução para obter o melhor cenário
      * COMPILACAO: COBOL CICS
      *----------------------------------------------------------------*
      * VRS0001 07.11.2016 - F2419497 - Implantacao.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. ARCSB628.

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*

      *    Variaveis de inicio do modulo
       77  CTE-INICIO-SS                PIC  X(35) VALUE
                   '*** ARCSB628 S.S. COMECA AQUI ***'.
       77  CTE-PROG                     PIC  X(18) VALUE
                                                    '*** ARCSB628 ***'.
       77  CTE-VERS                     PIC  X(06) VALUE 'VRS001'.
       77  SBVERSAO                     PIC  X(08) VALUE 'SBVERSAO'.

      *----------------------------------------------------------------*
       LOCAL-STORAGE SECTION.
      *----------------------------------------------------------------*

       01  GRP-ERRO.
           03  GD-EIBCALEN                  PIC  9(09).

       01  GRP-CTL-LS.
           03  IC-GR-RQSC                   PIC S9(04) COMP-5.
           03  IC-FXA-RQSC                  PIC S9(04) COMP-5.
           03  IC-FXA-RQSC-ANT              PIC S9(04) COMP-5.
           03  IC-GR-RQSC-D                 PIC  9(03).
           03  IC-FXA-RQSC-D                PIC  9(02).
           03  IC-GR1                       PIC  9(03).
           03  IC-GR2                       PIC  9(03).
           03  IC-GR3                       PIC  9(03).
           03  IC-GR4                       PIC  9(03).
           03  IC-GR5                       PIC  9(03).
           03  IC-GR6                       PIC  9(03).
           03  IC-GR7                       PIC  9(03).
           03  IC-GR8                       PIC  9(03).
           03  IC-GR9                       PIC  9(03).
           03  IC-GR10                      PIC  9(03).
           03  IC-FXA1                      PIC  9(02).
           03  IC-FXA2                      PIC  9(02).
           03  IC-FXA3                      PIC  9(02).
           03  IC-FXA4                      PIC  9(02).
           03  IC-FXA5                      PIC  9(02).
           03  IC-FXA6                      PIC  9(02).
           03  IC-FXA7                      PIC  9(02).
           03  IC-FXA8                      PIC  9(02).
           03  IC-FXA9                      PIC  9(02).
           03  IC-FXA10                     PIC  9(02).
           03  IC-RSTD                      PIC  9(03).
           03  IC-FXA-RSTD                  PIC  9(03).
           03  IC-GR                        PIC  9(03).
           03  IC-FXA                       PIC  9(02).
           03  QT-FXA                       PIC  9(02).

       01  GRP-CLC.
           03  MNR-VL                       PIC  9(15)V99
                                            VALUE 999999999999999,99.
           03  MOR-VL                       PIC  9(15)V99 VALUE ZEROS.
           03  MED-VL                       PIC  9(15)V99 VALUE ZEROS.
           03  VL-PC-ACI                    PIC  9(15)V99 VALUE ZEROS.
           03  VL-PC-ABXO                   PIC  9(15)V99 VALUE ZEROS.
           03  PC-ACI                       PIC  9(01)V9(4)
                                            VALUE 1,1000.
           03  PC-ABXO                      PIC  9(01)V9(4)
                                            VALUE 0,9000.
           03  ACM-AMTR                     PIC  9(15)V99.
           03  ACM-PCLD                     PIC  9(15)V99.
           03  MOR-PCLD                     PIC  9(15)V99 VALUE ZEROS.
           03  MOR-PCLD-80                  PIC  9(15)V99 VALUE ZEROS.
           03  MOR-PCLD-90                  PIC  9(15)V99 VALUE ZEROS.
           03  MOR-PCLD-110                 PIC  9(15)V99 VALUE ZEROS.
           03  MOR-PCLD-120                 PIC  9(15)V99 VALUE ZEROS.
           03  IC-ATU                       PIC  9(01)V9(16).
           03  MLHR-IC                      PIC  9(01)V9(16)
                                            VALUE 9,9999999999999999.
           03  MLHR-IC-10-ACI-ABXO             PIC  9(01)V9(16)
                                            VALUE 9,9999999999999999.
           03  VL-ENTD-80                   PIC  9(15)V99 VALUE ZEROS.
           03  VL-ENTD-90                   PIC  9(15)V99 VALUE ZEROS.
           03  VL-ENTD-110                  PIC  9(15)V99 VALUE ZEROS.
           03  VL-ENTD-120                  PIC  9(15)V99 VALUE ZEROS.

       01  GRP-RSTD-ATU.
           03  RSTD-ATU-QT-RSTD             PIC S9(04)  COMP-5 VALUE +0.
           03  RSTD-ATU-LS-CBN OCCURS 10 TIMES.
               05  RSTD-ATU-CD-GR-PRD       PIC S9(04)  COMP-5 VALUE +0.
               05  RSTD-ATU-CD-FXA-RVSA     PIC S9(04)  COMP-5 VALUE +0.

       01  FILLER                           PIC  X(32000).
       01  FILLER                           PIC  X(32000).
       01  FILLER                           PIC  X(32000).
       01  FILLER                           PIC  X(32000).
       01  FILLER                           PIC  X(32000).

       01  TABELA-RSTD.
           03  FILLER                       PIC  X(40)
                          VALUE 'Menor Amortizacao'.
           03  FILLER                       PIC  X(40)
                          VALUE 'Maior Amortizacao'.
           03  FILLER                       PIC  X(40)
                          VALUE 'Melhor Indice Amortizacao/PCLD'.
           03  FILLER                       PIC  X(40)
                          VALUE 'Melhor IC na faixa média +/- 10%'.
           03  FILLER                       PIC  X(40)
                          VALUE 'Melhor reversao PCLD com a entrada'.
           03  FILLER                       PIC  X(40)
                          VALUE 'Melhor reversao PCLD com -20% entrada'.
           03  FILLER                       PIC  X(40)
                          VALUE 'Melhor reversao PCLD com -10% entrada'.
           03  FILLER                       PIC  X(40)
                          VALUE 'Melhor reversao PCLD com +10% entrada'.
           03  FILLER                       PIC  X(40)
                          VALUE 'Melhor reversao PCLD com +20% entrada'.

       01  FILLER REDEFINES TABELA-RSTD.
           03  TAB-RSTD   OCCURS 9 TIMES    PIC  X(40).

       01  TX-DSCR-RSTD                     PIC  X(40).

       01  GRP-DISPLAY.
           03  TX-RSTD                      PIC  X(58).

       01  GR-FXA.
           03  CD-GR-PRD                    PIC  9(02).
           03  FXA-RVSA                     PIC  9(01).

      *----------------------------------------------------------------*
      * Book das subrotinas e operacoes chamadas
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * Área para montagem do display da input e resposta
      *----------------------------------------------------------------*

       01  DSP-RQSC.
           05  DSP-VL-ENTD                  PIC ZZZ.ZZ9,99.
           05  FILLER                       PIC  X(02) VALUE SPACES.
           05  DSP-QT-GR                    PIC ZZZZ9.
           05  DSP-LS-GR            OCCURS 10 TIMES.
               07  DSP-CD-GR-PRD            PIC ZZZZ9.
               07  FILLER                   PIC  X(02) VALUE SPACES.
               07  DSP-QT-FXA               PIC ZZZZ9.
               07  FILLER                   PIC  X(02) VALUE SPACES.
               07  DSP-LS-FXA-RVSA  OCCURS 8 TIMES.
                   09  DSP-CD-FXA-RVSA-RSCO PIC ZZZZ9.
                   09  FILLER               PIC X(02) VALUE SPACES.
                   09  DSP-VL-AMTR          PIC ZZZ.ZZ9,99.
                   09  FILLER               PIC X(02) VALUE SPACES.
                   09  DSP-VL-PCLD          PIC ZZZ.ZZ9,99.
                   09  FILLER               PIC X(02) VALUE SPACES.

       01  DSP-RPST.
           05  DSP-QT-RSTD                  PIC ZZZZ9.
           05  DSP-RSTD OCCURS 9 TIMES.
               07  DSP-QT-FXA-RSTD          PIC Z9.
               07  FILLER                   PIC X(02).
               07  DSP-LS-CBN OCCURS 10 TIMES.
                   09  FILLER               PIC X(02).
                   09  DSP-CD-GR-PRD-RSTD   PIC Z9.
                   09  FILLER               PIC X(01).
                   09  DSP-CD-FXA-RVSA-RSTD PIC Z9.
                   09  FILLER               PIC X(02).


      *----------------------------------------------------------------*
      *    Fim da LOCAL-STORAGE
      *----------------------------------------------------------------*
       77  CTE-FINAL-SS                  PIC X(40)      VALUE
                      '*** S.S. TERMINA AQUI ***'.

       LINKAGE SECTION.

       01  DFHCOMMAREA.
-INC ARCKB628

      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       000000-PRINCIPAL SECTION.
      *----------------------------------------------------------------*
      *    CALL SBVERSAO USING CTE-PROG CTE-VERS

           PERFORM 100000-PROCEDIMENTO-INICIAIS
           PERFORM 110000-VALIDAR-REQUISICAO
           PERFORM 200000-PROCESSAR
           PERFORM 300000-FINALIZAR
           .
       000000-SAI.
           GOBACK.

      *-----------------------------------------------------------------
       100000-PROCEDIMENTO-INICIAIS SECTION.
      *-----------------------------------------------------------------
           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               PERFORM 999001-ERRO
           END-IF
           DISPLAY CTE-PROG ' Inicio.: ' FUNCTION CURRENT-DATE

           MOVE ZEROS                   TO ARCSB628-QT-RSTD
           MOVE ZEROS                   TO ARCSB628-SEQL-ERRO
           MOVE SPACES                  TO ARCSB628-TX-ERRO

           PERFORM 990000-DISPLAY-INPUT
           .
       100000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       110000-VALIDAR-REQUISICAO         SECTION.
      *----------------------------------------------------------------*
           IF  ARCSB628-VL-ENTD NOT NUMERIC
               PERFORM 999008-ERRO
           END-IF

           IF  NOT ARCSB628-QT-GR-VLDO
               PERFORM 999002-ERRO
           END-IF

           PERFORM VARYING IC-GR-RQSC FROM 1 BY 1
           UNTIL IC-GR-RQSC GREATER ARCSB628-QT-GR

             IF  NOT ARCSB628-QT-FXA-VLDO(IC-GR-RQSC)
                 PERFORM 999003-ERRO
             END-IF

             IF ARCSB628-CD-GR-PRD(IC-GR-RQSC)
             LESS OR EQUAL ZEROS
                PERFORM 999004-ERRO
             END-IF

             MOVE ZEROS                 TO IC-FXA-RQSC-ANT

             PERFORM VARYING IC-FXA-RQSC FROM 1 BY 1
             UNTIL IC-FXA-RQSC GREATER ARCSB628-QT-FXA(IC-GR-RQSC)


                 IF ARCSB628-CD-FXA-RVSA-RSCO(IC-GR-RQSC IC-FXA-RQSC)
                 LESS OR EQUAL ZEROS
                    PERFORM 999005-ERRO
                 END-IF

                 IF ARCSB628-VL-AMTR(IC-GR-RQSC IC-FXA-RQSC) NOT NUMERIC
                 OR ARCSB628-VL-AMTR(IC-GR-RQSC IC-FXA-RQSC) EQUAL ZEROS
                    PERFORM 999006-ERRO
                 END-IF

                 IF ARCSB628-VL-PCLD(IC-GR-RQSC IC-FXA-RQSC) NOT NUMERIC
                 OR ARCSB628-VL-PCLD(IC-GR-RQSC IC-FXA-RQSC) EQUAL ZEROS
                    PERFORM 999007-ERRO
                 END-IF

                 IF ARCSB628-VL-AMTR(IC-GR-RQSC IC-FXA-RQSC) GREATER
                    ARCSB628-VL-PCLD(IC-GR-RQSC IC-FXA-RQSC)
                    PERFORM 999010-ERRO
                 END-IF

                 IF  IC-FXA-RQSC GREATER 1
                     IF ARCSB628-VL-AMTR(IC-GR-RQSC IC-FXA-RQSC) GREATER
                        ARCSB628-VL-AMTR(IC-GR-RQSC IC-FXA-RQSC-ANT)
                         CONTINUE
                     ELSE
                         PERFORM 999011-ERRO
                     END-IF

                     IF ARCSB628-VL-PCLD(IC-GR-RQSC IC-FXA-RQSC) GREATER
                        ARCSB628-VL-PCLD(IC-GR-RQSC IC-FXA-RQSC-ANT)
                         CONTINUE
                     ELSE
                         PERFORM 999012-ERRO
                     END-IF
                 END-IF

                 MOVE IC-FXA-RQSC       TO IC-FXA-RQSC-ANT

             END-PERFORM
           END-PERFORM
           .
       110000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
           PERFORM 250005-MNR-MOR-AMTZ

           MULTIPLY MED-VL BY PC-ABXO GIVING VL-PC-ABXO
           MULTIPLY MED-VL BY PC-ACI  GIVING VL-PC-ACI

           IF  ARCSB628-VL-ENTD EQUAL ZEROS
               MOVE 4                   TO ARCSB628-QT-RSTD
           ELSE
               MULTIPLY ARCSB628-VL-ENTD BY 0,8 GIVING VL-ENTD-80
               MULTIPLY ARCSB628-VL-ENTD BY 0,9 GIVING VL-ENTD-90
               MULTIPLY ARCSB628-VL-ENTD BY 1,1 GIVING VL-ENTD-110
               MULTIPLY ARCSB628-VL-ENTD BY 1,2 GIVING VL-ENTD-120
               MOVE 9                   TO ARCSB628-QT-RSTD
           END-IF

           PERFORM 250010-COMBINA-UM-A-UM

           .
       200000-SAI.
           EXIT.

      *----------------------------------------------------------------*
       250005-MNR-MOR-AMTZ SECTION.
      *----------------------------------------------------------------*
           MOVE 2                       TO ARCSB628-QT-RSTD
           MOVE 1                       TO ARCSB628-QT-FXA-RSTD(1)
           MOVE ARCSB628-QT-GR          TO ARCSB628-QT-FXA-RSTD(2)

           PERFORM VARYING IC-GR-RQSC FROM 1 BY 1
           UNTIL IC-GR-RQSC GREATER ARCSB628-QT-GR

      * Resultado 1: Grupo e faixa com menor valor de amortizacao
               IF  ARCSB628-VL-AMTR(IC-GR-RQSC 1) LESS MNR-VL
                   MOVE ARCSB628-VL-AMTR(IC-GR-RQSC 1)
                                        TO MNR-VL
                   MOVE ARCSB628-CD-GR-PRD(IC-GR-RQSC)
                                       TO ARCSB628-CD-GR-PRD-RSTD(1 1)
                   MOVE ARCSB628-CD-FXA-RVSA-RSCO(IC-GR-RQSC 1)
                                       TO ARCSB628-CD-FXA-RVSA-RSTD(1 1)
               END-IF

      * Resultado 2: Maior reversão possível
               MOVE ARCSB628-QT-FXA(IC-GR-RQSC) TO QT-FXA
               MOVE ARCSB628-CD-GR-PRD        (IC-GR-RQSC)
                                TO ARCSB628-CD-GR-PRD-RSTD(2 IC-GR-RQSC)
               MOVE ARCSB628-CD-FXA-RVSA-RSCO (IC-GR-RQSC QT-FXA)
                              TO ARCSB628-CD-FXA-RVSA-RSTD(2 IC-GR-RQSC)
               ADD ARCSB628-VL-AMTR           (IC-GR-RQSC QT-FXA)
                                        TO MOR-VL
           END-PERFORM

      * Valor médio de amortização. Será utilizado no Resultado 3
           COMPUTE MED-VL = (MNR-VL + MOR-VL) / 2
               ON SIZE ERROR PERFORM 999009-ERRO
           .
       250005-SAI.
           EXIT.

      *----------------------------------------------------------------*
       250010-COMBINA-UM-A-UM SECTION.
      *----------------------------------------------------------------*
           PERFORM VARYING IC-GR1 FROM 1 BY 1
           UNTIL IC-GR1 GREATER ARCSB628-QT-GR


               PERFORM VARYING IC-FXA1 FROM 1 BY 1
               UNTIL IC-FXA1 GREATER ARCSB628-QT-FXA(IC-GR1)
      *        Um a um
                   MOVE ZEROS           TO IC-FXA-RSTD
                                           ACM-AMTR
                                           ACM-PCLD
                                           RSTD-ATU-QT-RSTD

                   MOVE IC-GR1          TO IC-GR
                   MOVE IC-FXA1         TO IC-FXA
                   PERFORM 250300-MTA-RSTD-3-9

                   ADD 1 IC-GR1     GIVING IC-GR2
                   PERFORM 250020-COMBINA-DOIS-A-DOIS
               END-PERFORM

           END-PERFORM
           .
       250010-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250020-COMBINA-DOIS-A-DOIS SECTION.
      *----------------------------------------------------------------*
           PERFORM VARYING IC-GR2 FROM IC-GR2 BY 1
           UNTIL IC-GR2 GREATER ARCSB628-QT-GR

               ADD 1                    TO IC-FXA-RSTD

               PERFORM VARYING IC-FXA2 FROM 1 BY 1
               UNTIL IC-FXA2 GREATER ARCSB628-QT-FXA(IC-GR2)
      *        Dois a dois
                   MOVE IC-GR2          TO IC-GR
                   MOVE IC-FXA2         TO IC-FXA
                   PERFORM 250300-MTA-RSTD-3-9

                   ADD 1 IC-GR2     GIVING IC-GR3
                   PERFORM 250030-COMBINA-TRES-A-TRES
               END-PERFORM

           END-PERFORM
           .
       250020-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250030-COMBINA-TRES-A-TRES SECTION.
      *----------------------------------------------------------------*
           PERFORM VARYING IC-GR3 FROM IC-GR3 BY 1
           UNTIL IC-GR3 GREATER ARCSB628-QT-GR

               ADD 1                    TO IC-FXA-RSTD

               PERFORM VARYING IC-FXA3 FROM 1 BY 1
               UNTIL IC-FXA3 GREATER ARCSB628-QT-FXA(IC-GR3)
      *        Tres a tres
                   MOVE IC-GR3          TO IC-GR
                   MOVE IC-FXA3         TO IC-FXA
                   PERFORM 250300-MTA-RSTD-3-9

                   ADD 1 IC-GR3     GIVING IC-GR4
                   PERFORM 250040-COMBINA-QUATRO-A-QUATRO
               END-PERFORM

           END-PERFORM
           .
       250030-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250040-COMBINA-QUATRO-A-QUATRO SECTION.
      *----------------------------------------------------------------*
           PERFORM VARYING IC-GR4 FROM IC-GR4 BY 1
           UNTIL IC-GR4 GREATER ARCSB628-QT-GR

               ADD 1                    TO IC-FXA-RSTD

               PERFORM VARYING IC-FXA4 FROM 1 BY 1
               UNTIL IC-FXA4 GREATER ARCSB628-QT-FXA(IC-GR4)
      *        Quatro a quatro
                   MOVE IC-GR4          TO IC-GR
                   MOVE IC-FXA4         TO IC-FXA
                   PERFORM 250300-MTA-RSTD-3-9

                   ADD 1 IC-GR4     GIVING IC-GR5
                   PERFORM 250050-COMBINA-CINCO-A-CINCO
               END-PERFORM

           END-PERFORM
           .
       250040-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250050-COMBINA-CINCO-A-CINCO SECTION.
      *----------------------------------------------------------------*
           PERFORM VARYING IC-GR5 FROM IC-GR5 BY 1
           UNTIL IC-GR5 GREATER ARCSB628-QT-GR

               ADD 1                    TO IC-FXA-RSTD

               PERFORM VARYING IC-FXA5 FROM 1 BY 1
               UNTIL IC-FXA5 GREATER ARCSB628-QT-FXA(IC-GR5)
      *        Cinco a cinco
                   MOVE IC-GR5          TO IC-GR
                   MOVE IC-FXA5         TO IC-FXA
                   PERFORM 250300-MTA-RSTD-3-9

                   ADD 1 IC-GR5     GIVING IC-GR6
                   PERFORM 250060-COMBINA-SEIS-A-SEIS
               END-PERFORM

           END-PERFORM
           .
       250050-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250060-COMBINA-SEIS-A-SEIS SECTION.
      *----------------------------------------------------------------*
           PERFORM VARYING IC-GR6 FROM IC-GR6 BY 1
           UNTIL IC-GR6 GREATER ARCSB628-QT-GR

               ADD 1                    TO IC-FXA-RSTD

               PERFORM VARYING IC-FXA6 FROM 1 BY 1
               UNTIL IC-FXA6 GREATER ARCSB628-QT-FXA(IC-GR6)
      *        Seis a seis
                   MOVE IC-GR6          TO IC-GR
                   MOVE IC-FXA6         TO IC-FXA
                   PERFORM 250300-MTA-RSTD-3-9

                   ADD 1 IC-GR6     GIVING IC-GR7
                   PERFORM 250070-COMBINA-SETE-A-SETE
               END-PERFORM

           END-PERFORM
           .
       250060-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250070-COMBINA-SETE-A-SETE SECTION.
      *----------------------------------------------------------------*
           PERFORM VARYING IC-GR7 FROM IC-GR7 BY 1
           UNTIL IC-GR7 GREATER ARCSB628-QT-GR

               ADD 1                    TO IC-FXA-RSTD

               PERFORM VARYING IC-FXA7 FROM 1 BY 1
               UNTIL IC-FXA7 GREATER ARCSB628-QT-FXA(IC-GR7)
      *        Sete a sete
                   MOVE IC-GR7          TO IC-GR
                   MOVE IC-FXA7         TO IC-FXA
                   PERFORM 250300-MTA-RSTD-3-9

                   ADD 1 IC-GR7     GIVING IC-GR8
                   PERFORM 250080-COMBINA-OITO-A-OITO
               END-PERFORM

           END-PERFORM
           .
       250070-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250080-COMBINA-OITO-A-OITO SECTION.
      *----------------------------------------------------------------*
           PERFORM VARYING IC-GR8 FROM IC-GR8 BY 1
           UNTIL IC-GR8 GREATER ARCSB628-QT-GR

               ADD 1                    TO IC-FXA-RSTD

               PERFORM VARYING IC-FXA8 FROM 1 BY 1
               UNTIL IC-FXA8 GREATER ARCSB628-QT-FXA(IC-GR8)
      *        Oito a oito
                   MOVE IC-GR8          TO IC-GR
                   MOVE IC-FXA8         TO IC-FXA
                   PERFORM 250300-MTA-RSTD-3-9

                   ADD 1 IC-GR8     GIVING IC-GR9
                   PERFORM 250090-COMBINA-NOVE-A-NOVE
               END-PERFORM

           END-PERFORM
           .
       250080-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250090-COMBINA-NOVE-A-NOVE SECTION.
      *----------------------------------------------------------------*
           PERFORM VARYING IC-GR9 FROM IC-GR9 BY 1
           UNTIL IC-GR9 GREATER ARCSB628-QT-GR

               ADD 1                    TO IC-FXA-RSTD

               PERFORM VARYING IC-FXA9 FROM 1 BY 1
               UNTIL IC-FXA9 GREATER ARCSB628-QT-FXA(IC-GR9)
      *        Nove a nove
                   MOVE IC-GR9          TO IC-GR
                   MOVE IC-FXA9         TO IC-FXA
                   PERFORM 250300-MTA-RSTD-3-9

                   ADD 1 IC-GR9     GIVING IC-GR10
                   PERFORM 250100-COMBINA-DEZ-A-DEZ
               END-PERFORM

           END-PERFORM
           .
       250090-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250100-COMBINA-DEZ-A-DEZ SECTION.
      *----------------------------------------------------------------*
           PERFORM VARYING IC-GR10 FROM IC-GR10 BY 1
           UNTIL IC-GR10 GREATER ARCSB628-QT-GR

               ADD 1                    TO IC-FXA-RSTD

               PERFORM VARYING IC-FXA10 FROM 1 BY 1
               UNTIL IC-FXA10 GREATER ARCSB628-QT-FXA(IC-GR10)
      *        Dez a dez
                   MOVE IC-GR10         TO IC-GR
                   MOVE IC-FXA10        TO IC-FXA
                   PERFORM 250300-MTA-RSTD-3-9

               END-PERFORM

           END-PERFORM
           .
       250100-SAI.
           EXIT.

      *----------------------------------------------------------------*
       250300-MTA-RSTD-3-9 SECTION.
      *----------------------------------------------------------------*
           ADD 1                              TO RSTD-ATU-QT-RSTD
           MOVE ARCSB628-CD-GR-PRD                (IC-GR)
                       TO RSTD-ATU-CD-GR-PRD      (RSTD-ATU-QT-RSTD)
           MOVE ARCSB628-CD-FXA-RVSA-RSCO         (IC-GR IC-FXA)
                       TO RSTD-ATU-CD-FXA-RVSA    (RSTD-ATU-QT-RSTD)

           ADD ARCSB628-VL-AMTR(IC-GR IC-FXA) TO ACM-AMTR
           ADD ARCSB628-VL-PCLD(IC-GR IC-FXA) TO ACM-PCLD
           DIVIDE ACM-AMTR BY ACM-PCLD GIVING IC-ATU

      *    Resultado 3 - Melhor IC
           PERFORM 250500-CLC-MLHR-IC

      *    Resultado 4 - Melhor IC na faixa +/- 10% da média de
      *    amortização
           IF  VL-PC-ABXO LESS ACM-AMTR
           AND VL-PC-ACI GREATER ACM-AMTR
               PERFORM 250600-CLC-MLHR-IC-10-ACI-ABXO
           END-IF

           IF  ARCSB628-VL-ENTD EQUAL ZEROS
               GO TO 250300-SAI
           END-IF

      *    Resultado 5 - Maior reversao para a entrada
           IF  ACM-AMTR LESS OR EQUAL ARCSB628-VL-ENTD
               PERFORM 250700-CLC-MLHR-RVSA-PVS-DUVS
           END-IF

      *    Resultado 6 - Maior reversao para 80% da entrada
           IF  ACM-AMTR LESS OR EQUAL VL-ENTD-80
               PERFORM 250710-CLC-MLHR-RVSA-PCLD-80
           END-IF

      *    Resultado 7 - Maior reversao para 90% da entrada
           IF  ACM-AMTR LESS OR EQUAL VL-ENTD-90
               PERFORM 250720-CLC-MLHR-RVSA-PCLD-90
           END-IF

      *    Resultado 8 - Maior reversao para 110% da entrada
           IF  ACM-AMTR LESS OR EQUAL VL-ENTD-110
               PERFORM 250730-CLC-MLHR-RVSA-PCLD-110
           END-IF

      *    Resultado 9 - Maior reversao para 120% da entrada
           IF  ACM-AMTR LESS OR EQUAL VL-ENTD-120
               PERFORM 250740-CLC-MLHR-RVSA-PCLD-120
           END-IF
           .
       250300-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250500-CLC-MLHR-IC SECTION.
      *----------------------------------------------------------------*
           IF  IC-ATU LESS MLHR-IC
               MOVE IC-ATU              TO MLHR-IC
               MOVE GRP-RSTD-ATU        TO ARCSB628-RSTD(3)
           END-IF
           .
       250500-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250600-CLC-MLHR-IC-10-ACI-ABXO SECTION.
      *----------------------------------------------------------------*
           IF  IC-ATU LESS MLHR-IC-10-ACI-ABXO
               MOVE IC-ATU              TO MLHR-IC-10-ACI-ABXO
               MOVE GRP-RSTD-ATU        TO ARCSB628-RSTD(4)
           END-IF
           .
       250600-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250700-CLC-MLHR-RVSA-PVS-DUVS SECTION.
      *----------------------------------------------------------------*
           IF  ACM-PCLD GREATER MOR-PCLD
               MOVE ACM-PCLD            TO MOR-PCLD
               MOVE GRP-RSTD-ATU        TO ARCSB628-RSTD(5)
           END-IF
           .
       250700-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250710-CLC-MLHR-RVSA-PCLD-80 SECTION.
      *----------------------------------------------------------------*
           IF  ACM-PCLD GREATER MOR-PCLD-80
               MOVE ACM-PCLD            TO MOR-PCLD-80
               MOVE GRP-RSTD-ATU        TO ARCSB628-RSTD(6)
           END-IF
           .
       250710-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250720-CLC-MLHR-RVSA-PCLD-90 SECTION.
      *----------------------------------------------------------------*
           IF  ACM-PCLD GREATER MOR-PCLD-90
               MOVE ACM-PCLD            TO MOR-PCLD-90
               MOVE GRP-RSTD-ATU        TO ARCSB628-RSTD(7)
           END-IF
           .
       250720-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250730-CLC-MLHR-RVSA-PCLD-110 SECTION.
      *----------------------------------------------------------------*
           IF  ACM-PCLD GREATER MOR-PCLD-110
               MOVE ACM-PCLD            TO MOR-PCLD-110
               MOVE GRP-RSTD-ATU        TO ARCSB628-RSTD(8)
           END-IF
           .
       250730-SAI.
           EXIT.
      *----------------------------------------------------------------*
       250740-CLC-MLHR-RVSA-PCLD-120 SECTION.
      *----------------------------------------------------------------*
           IF  ACM-PCLD GREATER MOR-PCLD-120
               MOVE ACM-PCLD            TO MOR-PCLD-120
               MOVE GRP-RSTD-ATU        TO ARCSB628-RSTD(9)
           END-IF
           .
       250740-SAI.
           EXIT.
      *----------------------------------------------------------------*
       300000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*
           DISPLAY CTE-PROG ' Resultado...'

           PERFORM 990100-DISPLAY-RESPOSTA

           DISPLAY CTE-PROG ' Fim....: ' FUNCTION CURRENT-DATE
           .
       300000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       990000-DISPLAY-INPUT SECTION.
      *-----------------------------------------------------------------
           DISPLAY '   Entrada Qt Gr'
           MOVE ARCSB628-VL-ENTD        TO DSP-VL-ENTD
           MOVE ARCSB628-QT-GR          TO DSP-QT-GR
           DISPLAY DSP-VL-ENTD ' ' DSP-QT-GR

           DISPLAY 'Cd Gr  Qt Fx  '
           'Cd Fx     Vl Amtr     Vl PCLD  '
           'Cd Fx     Vl Amtr     Vl PCLD  '
           'Cd Fx     Vl Amtr     Vl PCLD  '
           'Cd Fx     Vl Amtr     Vl PCLD  '
           'Cd Fx     Vl Amtr     Vl PCLD  '
           'Cd Fx     Vl Amtr     Vl PCLD  '
           'Cd Fx     Vl Amtr     Vl PCLD  '
           'Cd Fx     Vl Amtr     Vl PCLD'

           PERFORM VARYING IC-GR-RQSC FROM 1 BY 1
           UNTIL IC-GR-RQSC GREATER ARCSB628-QT-GR

             MOVE SPACES                TO DSP-LS-GR (IC-GR-RQSC)
             MOVE ARCSB628-CD-GR-PRD(IC-GR-RQSC)
                                        TO DSP-CD-GR-PRD(IC-GR-RQSC)
             MOVE ARCSB628-QT-FXA(IC-GR-RQSC)
                                        TO DSP-QT-FXA(IC-GR-RQSC)

             PERFORM VARYING IC-FXA-RQSC FROM 1 BY 1
             UNTIL IC-FXA-RQSC GREATER ARCSB628-QT-FXA(IC-GR-RQSC)

                 MOVE ARCSB628-CD-FXA-RVSA-RSCO(IC-GR-RQSC IC-FXA-RQSC)
                        TO DSP-CD-FXA-RVSA-RSCO(IC-GR-RQSC IC-FXA-RQSC)
                 MOVE ARCSB628-VL-AMTR(IC-GR-RQSC IC-FXA-RQSC)
                        TO DSP-VL-AMTR(IC-GR-RQSC IC-FXA-RQSC)
                 MOVE ARCSB628-VL-PCLD(IC-GR-RQSC IC-FXA-RQSC)
                        TO DSP-VL-PCLD(IC-GR-RQSC IC-FXA-RQSC)

             END-PERFORM

             DISPLAY DSP-LS-GR (IC-GR-RQSC)

           END-PERFORM
           .
       990000-SAI.
           EXIT.

      *-----------------------------------------------------------------
       990100-DISPLAY-RESPOSTA SECTION.
      *-----------------------------------------------------------------

           MOVE SPACES                  TO TX-RSTD

           DISPLAY TX-RSTD 'Qt Fx  '
           'Gr Fx  '
           'Gr Fx  '
           'Gr Fx  '
           'Gr Fx  '
           'Gr Fx  '
           'Gr Fx  '
           'Gr Fx  '
           'Gr Fx  '
           'Gr Fx  '
           'Gr Fx  '
           'Gr Fx'

           PERFORM VARYING IC-RSTD FROM 1 BY 1
           UNTIL IC-RSTD GREATER ARCSB628-QT-RSTD

               MOVE SPACES              TO DSP-RSTD(IC-RSTD)
               MOVE TAB-RSTD(IC-RSTD)   TO TX-DSCR-RSTD

               STRING
                   'Resultado ' IC-RSTD ' - ' TX-DSCR-RSTD ': '
                   DELIMITED BY SIZE INTO TX-RSTD
               END-STRING

               MOVE ARCSB628-QT-FXA-RSTD(IC-RSTD)
                            TO DSP-QT-FXA-RSTD(IC-RSTD)

               PERFORM VARYING IC-FXA-RSTD FROM 1 BY 1
               UNTIL IC-FXA-RSTD GREATER ARCSB628-QT-FXA-RSTD(IC-RSTD)


                   MOVE ARCSB628-CD-GR-PRD-RSTD(IC-RSTD IC-FXA-RSTD)
                            TO DSP-CD-GR-PRD-RSTD(IC-RSTD IC-FXA-RSTD)
                   MOVE ARCSB628-CD-FXA-RVSA-RSTD(IC-RSTD IC-FXA-RSTD)
                            TO DSP-CD-FXA-RVSA-RSTD(IC-RSTD IC-FXA-RSTD)
               END-PERFORM

               DISPLAY TX-RSTD ' ' DSP-RSTD (IC-RSTD)

           END-PERFORM

           .
       990100-SAI.
           EXIT.
      *-----------------------------------------------------------------
       999000-ERRO SECTION.
      *-----------------------------------------------------------------

       999001-ERRO.
      *------------
           MOVE 0001                    TO ARCSB628-SEQL-ERRO
           MOVE EIBCALEN                TO GD-EIBCALEN
           STRING
               'Tamanho do parametro invalido. '
               'Esperado: ' GD-EIBCALEN ' | '
               'Informado: ' LENGTH OF DFHCOMMAREA
               DELIMITED BY SIZE     INTO ARCSB628-TX-ERRO
           END-STRING
           PERFORM 000000-SAI
           .
       999002-ERRO.
      *------------
           MOVE 0002                    TO ARCSB628-SEQL-ERRO
           MOVE 'Quantidade de grupos invalida'
                                        TO ARCSB628-TX-ERRO
           PERFORM 000000-SAI
           .
       999003-ERRO.
      *------------
           MOVE 0003                    TO ARCSB628-SEQL-ERRO
           MOVE IC-GR-RQSC              TO IC-GR-RQSC-D
           STRING
              'Quantidade de faixas para o grupo  da ocorrencia '
              IC-GR-RQSC-D ' invalida' DELIMITED BY SIZE
                                      INTO ARCSB628-TX-ERRO
           PERFORM 000000-SAI
           .
       999004-ERRO.
      *------------
           MOVE 0004                    TO ARCSB628-SEQL-ERRO
           MOVE IC-GR-RQSC              TO IC-GR-RQSC-D
           MOVE IC-FXA-RQSC             TO IC-FXA-RQSC-D
           STRING
              'Codigo do grupo invalido. Ocorrencia do grupo: '
              IC-GR-RQSC-D ' | Ocorrencia da faixa: ' IC-FXA-RQSC-D
              DELIMITED BY SIZE       INTO ARCSB628-TX-ERRO
           END-STRING
           PERFORM 000000-SAI
           .
       999005-ERRO.
      *------------
           MOVE 0005                    TO ARCSB628-SEQL-ERRO
           MOVE IC-GR-RQSC              TO IC-GR-RQSC-D
           MOVE IC-FXA-RQSC             TO IC-FXA-RQSC-D
           STRING
              'Codigo da faixa invalido. Ocorrencia do grupo: '
              IC-GR-RQSC-D ' | Ocorrencia da faixa: ' IC-FXA-RQSC-D
              DELIMITED BY SIZE       INTO ARCSB628-TX-ERRO
           END-STRING
           PERFORM 000000-SAI
           .
       999006-ERRO.
      *------------
           MOVE 0006                    TO ARCSB628-SEQL-ERRO
           MOVE IC-GR-RQSC              TO IC-GR-RQSC-D
           MOVE IC-FXA-RQSC             TO IC-FXA-RQSC-D
           STRING
              'Valor a amortizar invalido. Ocorrencia do grupo: '
              IC-GR-RQSC-D ' | Ocorrencia da faixa: ' IC-FXA-RQSC-D
              DELIMITED BY SIZE       INTO ARCSB628-TX-ERRO
           END-STRING
           PERFORM 000000-SAI
           .
       999007-ERRO.
      *------------
           MOVE 0007                    TO ARCSB628-SEQL-ERRO
           MOVE IC-GR-RQSC              TO IC-GR-RQSC-D
           MOVE IC-FXA-RQSC             TO IC-FXA-RQSC-D
           STRING
              'Valor da PCLD invalido. Ocorrencia do grupo: '
              IC-GR-RQSC-D ' | Ocorrencia da faixa: ' IC-FXA-RQSC-D
              DELIMITED BY SIZE       INTO ARCSB628-TX-ERRO
           END-STRING
           PERFORM 000000-SAI
           .
       999008-ERRO.
      *------------
           MOVE 0008                    TO ARCSB628-SEQL-ERRO
           MOVE 'Valor da entrada invalido'
                                        TO ARCSB628-TX-ERRO
           PERFORM 000000-SAI
           .
       999009-ERRO.
      *------------
           MOVE 0009                    TO ARCSB628-SEQL-ERRO
           MOVE 'Erro ao calcular a media das amortizacoes'
                                        TO ARCSB628-TX-ERRO
           PERFORM 000000-SAI
           .
       999010-ERRO.
      *------------
           MOVE 0010                    TO ARCSB628-SEQL-ERRO
           MOVE IC-GR-RQSC              TO IC-GR-RQSC-D
           MOVE IC-FXA-RQSC             TO IC-FXA-RQSC-D
           STRING
              'Valor da amortizacao maior que PCLD. '
              'Ocorrencia do grupo: ' IC-GR-RQSC-D ' | '
              'Ocorrencia da faixa: ' IC-FXA-RQSC-D
              DELIMITED BY SIZE       INTO ARCSB628-TX-ERRO
           END-STRING
           PERFORM 000000-SAI
           .
       999011-ERRO.
      *------------
           MOVE 0011                    TO ARCSB628-SEQL-ERRO
           MOVE IC-GR-RQSC              TO IC-GR-RQSC-D
           MOVE IC-FXA-RQSC             TO IC-FXA-RQSC-D
           STRING
              'Valor da amortizacao da faixa atual menor ou igual '
              'a amortizacao da faixa anterior. '
              'Ocorrencia do grupo: ' IC-GR-RQSC-D ' | '
              'Ocorrencia da faixa: ' IC-FXA-RQSC-D
              DELIMITED BY SIZE       INTO ARCSB628-TX-ERRO
           END-STRING
           PERFORM 000000-SAI
           .
       999012-ERRO.
      *------------
           MOVE 0012                    TO ARCSB628-SEQL-ERRO
           MOVE IC-GR-RQSC              TO IC-GR-RQSC-D
           MOVE IC-FXA-RQSC             TO IC-FXA-RQSC-D
           STRING
              'Valor da PCLD da faixa atual menor ou igual '
              'a PCLD da faixa anterior. '
              'Ocorrencia do grupo: ' IC-GR-RQSC-D ' | '
              'Ocorrencia da faixa: ' IC-FXA-RQSC-D
              DELIMITED BY SIZE       INTO ARCSB628-TX-ERRO
           END-STRING
           PERFORM 000000-SAI
           .
       999000-SAI.
           EXIT.