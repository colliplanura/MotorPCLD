      *---------------AX--------------------------------------------------
      * PROGRAMA..: ARCP0627
      * ANALISTA..: F8944859 - SANDRO FERNANDES COLLI DA SILVA
      * AUTOR.....: F8944859 - SANDRO FERNANDES COLLI DA SILVA
      * OBJETIVO..: Recebe requisicao via fila
      * COMPILACAO: COBOL MVS
      *-----------------------------------------------------------------
      * VRS001 21.10.2016 - F8944859 - IMPLANTACAO
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. ARCP0627.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  CTE-PROG     PIC  X(17) VALUE '*** ARCP0627 ***'.
       01  CTE-VERS     PIC  X(06) VALUE 'VRS001'.
       77  SBVERSAO     PIC  X(08) VALUE 'SBVERSAO'.
       77  ARCSB627     PIC  X(08) VALUE 'ARCSB627'.
       77  ARCSB628     PIC  X(08) VALUE 'ARCSB628'.
       77  BBDS0099     PIC  X(08) VALUE 'BBDS0099'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
       77  ALEATORIO                    PIC  9(10).
       77  ALEATORIO2                   PIC  9(10).
       01  GRP-TAB.
           03  IX-GR                    PIC  9(02).
           03  IX-FXA                   PIC  9(01).
           03  IX-FXA-ANT               PIC  9(01).
           03  IC-RSTD                  PIC  9(01).
           03  IC-FXA-RSTD              PIC  9(02).
           03  MAX-PCLD                 PIC S9(15)V99 VALUE 5000.

       01  DATA-HORA.
           03  FILLER                   PIC  X(08).
           03  HORA                     PIC  9(08).
           03  FILLER                   PIC  X(05).

       01  PARM1.
           03  FILLER                  PIC  X(02).
           03  PARM-GR                 PIC  9(02) VALUE 2.
           03  FILLER                  PIC  X(01).
           03  PARM-FXA                PIC  9(01) VALUE 8.
           03  FILLER                  PIC  X(01).
           03  PARM-ENTD               PIC  9(15)V99 VALUE 10000,00.

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

       01  ARCSB628-DADOS.
       COPY ARCKB628
      *-INC ARCKB628
           .
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       000000-PRINCIPAL SECTION.
      *-----------------------------------------------------------------

      *    CALL SBVERSAO USING CTE-PROG CTE-VERS

           DISPLAY CTE-PROG 'PARM1: ' PARM1
           DISPLAY CTE-PROG 'PARM-GR: ' PARM-GR ' | PARM-FXA: ' PARM-FXA

           MOVE FUNCTION CURRENT-DATE   TO DATA-HORA
           COMPUTE ALEATORIO = FUNCTION RANDOM(HORA)
               ON SIZE ERROR DISPLAY '.'
           END-COMPUTE
           DISPLAY 'ALEATORIO: ' ALEATORIO


           IF  PARM-ENTD NOT NUMERIC
               MOVE ZEROS               TO PARM-ENTD
           END-IF

           MOVE PARM-ENTD               TO ARCSB628-VL-ENTD
           MOVE PARM-GR                 TO ARCSB628-QT-GR

           PERFORM VARYING IX-GR FROM 1 BY 1
           UNTIL IX-GR GREATER ARCSB628-QT-GR

               COMPUTE ARCSB628-QT-FXA(IX-GR) = FUNCTION RANDOM *
                                                (PARM-FXA - 1) + 1
                  ON SIZE ERROR DISPLAY '.' 
               END-COMPUTE

               MOVE IX-GR  TO ARCSB628-CD-GR-PRD (IX-GR)

               MOVE 0 TO IX-FXA-ANT

               PERFORM VARYING IX-FXA FROM 1 BY 1
               UNTIL IX-FXA GREATER ARCSB628-QT-FXA(IX-GR)

                   MOVE IX-FXA
                             TO ARCSB628-CD-FXA-RVSA-RSCO(IX-GR IX-FXA)
                   IF  IX-FXA EQUAL 1
                       COMPUTE ARCSB628-VL-PCLD(IX-GR IX-FXA) =
                           FUNCTION RANDOM * MAX-PCLD
                           ON SIZE ERROR DISPLAY '.'
                       END-COMPUTE

                       COMPUTE ARCSB628-VL-AMTR(IX-GR IX-FXA) =
                           FUNCTION RANDOM *
                           (ARCSB628-VL-PCLD(IX-GR IX-FXA) - 1) + 1
                           ON SIZE ERROR DISPLAY '.'
                       END-COMPUTE
                   ELSE
                       COMPUTE ARCSB628-VL-PCLD(IX-GR IX-FXA) =
                           FUNCTION RANDOM * (MAX-PCLD -
                           ARCSB628-VL-PCLD(IX-GR IX-FXA-ANT)) +
                           ARCSB628-VL-PCLD(IX-GR IX-FXA-ANT)
                           ON SIZE ERROR DISPLAY '.'
                       END-COMPUTE

                       COMPUTE ARCSB628-VL-AMTR(IX-GR IX-FXA) =
                           FUNCTION RANDOM *
                           (ARCSB628-VL-PCLD(IX-GR IX-FXA) -
                           ARCSB628-VL-AMTR(IX-GR IX-FXA-ANT)) +
                           ARCSB628-VL-AMTR(IX-GR IX-FXA-ANT)
                           ON SIZE ERROR DISPLAY '.'
                       END-COMPUTE
                   END-IF

                   MOVE IX-FXA TO IX-FXA-ANT

               END-PERFORM

           END-PERFORM

           MOVE ZEROS                   TO ARCSB628-QT-RSTD
           MOVE ZEROS                   TO ARCSB628-SEQL-ERRO
           MOVE SPACES                  TO ARCSB628-TX-ERRO

      *    MOVE 999                     TO ARCSB628-SEQL-ERRO
      *    MOVE 'Sandro Colli'          TO ARCSB628-TX-ERRO

      *    DISPLAY CTE-PROG ' Prog C '
      *    PERFORM 990000-DISPLAY-INPUT
      *    CALL BBDS0099             USING ARCSB628-DADOS

      *    DISPLAY CTE-PROG ' 628-SEQL-ER: ' ARCSB628-SEQL-ERRO
      *    DISPLAY CTE-PROG ' 628-TX-ERRO: ' ARCSB628-TX-ERRO
      *    DISPLAY CTE-PROG ' -----------------------------------'
      *    PERFORM 990100-DISPLAY-RESPOSTA
      *    DISPLAY CTE-PROG ' -----------------------------------'

           DISPLAY CTE-PROG ' Prog Cobol '
           PERFORM 990000-DISPLAY-INPUT
           CALL ARCSB628             USING ARCSB628-DADOS

           DISPLAY CTE-PROG ' 628-SEQL-ER: ' ARCSB628-SEQL-ERRO
           DISPLAY CTE-PROG ' 628-TX-ERRO: ' ARCSB628-TX-ERRO
           DISPLAY CTE-PROG ' -----------------------------------'
           PERFORM 990100-DISPLAY-RESPOSTA
           DISPLAY CTE-PROG ' -----------------------------------'


           .
       000000-SAI.
           STOP RUN
           .
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

           PERFORM VARYING IX-GR FROM 1 BY 1
           UNTIL IX-GR GREATER ARCSB628-QT-GR

             MOVE SPACES                TO DSP-LS-GR (IX-GR)
             MOVE ARCSB628-CD-GR-PRD(IX-GR)
                                        TO DSP-CD-GR-PRD(IX-GR)
             MOVE ARCSB628-QT-FXA(IX-GR)
                                        TO DSP-QT-FXA(IX-GR)

             PERFORM VARYING IX-FXA FROM 1 BY 1
             UNTIL IX-FXA GREATER ARCSB628-QT-FXA(IX-GR)

                 MOVE ARCSB628-CD-FXA-RVSA-RSCO(IX-GR IX-FXA)
                        TO DSP-CD-FXA-RVSA-RSCO(IX-GR IX-FXA)
                 MOVE ARCSB628-VL-AMTR(IX-GR IX-FXA)
                        TO DSP-VL-AMTR(IX-GR IX-FXA)
                 MOVE ARCSB628-VL-PCLD(IX-GR IX-FXA)
                        TO DSP-VL-PCLD(IX-GR IX-FXA)

             END-PERFORM

             DISPLAY DSP-LS-GR (IX-GR)

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
           MOVE 888                     TO RETURN-CODE
           DISPLAY CTE-PROG ' 888 - Erro'
           .
       999000-SAI.
           EXIT.
