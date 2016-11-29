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
       INPUT-OUTPUT SECTION.
      *-----------------------------------------------------------------
       FILE-CONTROL.
      *-------------
               SELECT  ENTRADA  ASSIGN  TO  UT-S-ENTRADA.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       FILE SECTION.
      *-----------------------------------------------------------------
       FD  ENTRADA
           BLOCK 0 RECORDS
           RECORD  33
           RECORDING MODE IS F.

       01  ENTRADA-REGISTRO.
           03  ENTRADA-SIS             PIC  X(03).
           03  FILLER                  PIC  X(01).
           03  ENTRADA-OPR             PIC  9(17).
           03  FILLER                  PIC  X(01).
           03  ENTRADA-EPRD            PIC  9(05).
           03  FILLER                  PIC  X(01).
           03  ENTRADA-SCTR            PIC  9(05).
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
           03  IX-RQSC                  PIC S9(05) COMP-5.
           03  FIM-ARQ                  PIC  X(01) VALUE 'N'.
               88  IN-FIM                          VALUE 'S'.
           03  IX-GR                    PIC  9(02).
           03  IX-FXA                   PIC  9(01).
           03  IX-FXA-ANT               PIC  9(01).
           03  MAX-PCLD                 PIC S9(15)V99 VALUE 5000.

       01  DATA-HORA.
           03  FILLER                   PIC  X(08).
           03  HORA                     PIC  9(08).
           03  FILLER                   PIC  X(05).



-INC HLPKDFHE
       01  ARCSB627-DADOS.
-INC ARCKB627

       01  ARCSB628-DADOS.
-INC ARCKB628

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
       01  PARM1.
           03  FILLER                  PIC  X(02).
           03  PARM-GR                 PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  PARM-FXA                PIC  9(01).
           03  FILLER                  PIC  X(01).
           03  PARM-ENTD               PIC  9(15)V99.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING PARM1.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       000000-PRINCIPAL SECTION.
      *-----------------------------------------------------------------

           CALL SBVERSAO USING CTE-PROG CTE-VERS

           DISPLAY CTE-PROG 'PARM1: ' PARM1
           DISPLAY CTE-PROG 'PARM-GR: ' PARM-GR ' | PARM-FXA: ' PARM-FXA

           MOVE FUNCTION CURRENT-DATE   TO DATA-HORA
           COMPUTE ALEATORIO = FUNCTION RANDOM(HORA)
               ON SIZE ERROR DISPLAY '.'
           END-COMPUTE
           DISPLAY 'ALEATORIO: ' ALEATORIO
      *    PERFORM 1000 TIMES
      *    COMPUTE ALEATORIO = FUNCTION RANDOM * (100 - 10 + 1) + 10
      *        ON SIZE ERROR DISPLAY '.' END-COMPUTE
      *    COMPUTE ALEATORIO2 = FUNCTION RANDOM * (100 - 10) + 10
      *        ON SIZE ERROR DISPLAY '.' END-COMPUTE
      *    DISPLAY CTE-PROG 'RANDOM: ' ALEATORIO ALEATORIO2
      *    END-PERFORM

      *    OPEN INPUT ENTRADA.
      *    READ ENTRADA AT END
      *       DISPLAY CTE-PROG ' Sem registros de entrada'
      *       GO TO 000000-SAI
      *    END-READ
      *
      *    PERFORM UNTIL IN-FIM
      *        PERFORM VARYING IX-RQSC FROM 1 BY 1
      *        UNTIL IN-FIM
      *
      *          MOVE IX-RQSC      TO ARCSB627-QT-OPR
      *          MOVE ENTRADA-SIS  TO ARCSB627-SG-SIS-OGM-OPR (IX-RQSC)
      *          MOVE ENTRADA-OPR  TO ARCSB627-NR-UNCO-CTR-OPR(IX-RQSC)
      *          MOVE ENTRADA-EPRD TO ARCSB627-NR-EPRD-FNCD   (IX-RQSC)
      *          MOVE ENTRADA-SCTR TO ARCSB627-NR-SCTR-OPR    (IX-RQSC)
      *
      *          READ ENTRADA AT END MOVE 'S' TO FIM-ARQ END-READ
      *
      *        END-PERFORM
      *
      *        MOVE LENGTH OF ARCSB627-DADOS TO EIBCALEN
      *
      *        CALL ARCSB627             USING DFHEIBLK ARCSB627-DADOS
      *
      *        DISPLAY CTE-PROG ' 627-IN-SCS : ' ARCSB627-IN-SCS
      *        DISPLAY CTE-PROG ' 627-SEQL-ER: ' ARCSB627-SEQL-ERRO
      *        DISPLAY CTE-PROG ' 627-TX-ERRO: ' ARCSB627-TX-ERRO
      *        DISPLAY CTE-PROG ' -----------------------------------'
      *    END-PERFORM

           IF  PARM-ENTD NOT NUMERIC
               MOVE ZEROS               TO PARM-ENTD
           END-IF

           MOVE PARM-ENTD               TO ARCSB628-VL-ENTD
           MOVE PARM-GR                 TO ARCSB628-QT-GR

           PERFORM VARYING IX-GR FROM 1 BY 1
           UNTIL IX-GR GREATER ARCSB628-QT-GR

               COMPUTE ARCSB628-QT-FXA(IX-GR) = FUNCTION RANDOM *
                                                (PARM-FXA - 1) + 1
                  ON SIZE ERROR DISPLAY '.' END-COMPUTE

      *        MOVE PARM-FXA               TO ARCSB628-QT-FXA(IX-GR)
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

           DISPLAY CTE-PROG ' Prog C '
      *    MOVE LENGTH OF ARCSB628-DADOS TO EIBCALEN

           CALL BBDS0099             USING ARCSB627-DADOS

           DISPLAY CTE-PROG ' Prog Cobol '
           MOVE LENGTH OF ARCSB628-DADOS TO EIBCALEN
           CALL ARCSB628             USING DFHEIBLK ARCSB628-DADOS

           DISPLAY CTE-PROG ' 628-SEQL-ER: ' ARCSB628-SEQL-ERRO
           DISPLAY CTE-PROG ' 628-TX-ERRO: ' ARCSB628-TX-ERRO
           DISPLAY CTE-PROG ' -----------------------------------'


           .
       000000-SAI.
           STOP RUN
           .
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