       IDENTIFICATION DIVISION.
       PROGRAM-ID. RODA.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CTE-PROG PIC  X(08)    VALUE 'RODA'.
       01  CTE-VERS PIC  X(06)    VALUE 'VRS001'.
       77  MAX-PCLD PIC S9(15)V99 VALUE 5000.
       77  COMBINA  PIC  X(08)    VALUE 'combina'.
       
       LOCAL-STORAGE SECTION.
       01  GRP-LS.
           03  IC-GR                    PIC S9(04)    COMP-5.
           03  TOT-GR-RQSC              PIC S9(04)    COMP-5.
           03  IC-FXA                   PIC S9(04)    COMP-5.
           03  IC-FXA-ANT               PIC S9(04)    COMP-5.
           03  IC-RSTD                  PIC S9(04)    COMP-5.
           03  IC-FXA-RSTD              PIC S9(04)    COMP-5.
           
       01  GRP-ERRO.
           03  GDA-RETURN-CODE          PIC  9(09).
       
       01  DATA-HORA.
           03  FILLER                   PIC  X(08).
           03  HORA                     PIC  9(08).
           03  FILLER                   PIC  X(05).
       
       01  BOOK.
           03  ERRO.
               05  SEQL-ERRO            PIC S9(09)    COMP-5.
               05  TX-ERRO              PIC  X(120).
           03  RQSC.
               05  VL-ENTD              PIC S9(15)V99 COMP-3.
               05  QT-GR                PIC S9(04)    COMP-5.
               05  LS-GR OCCURS 10 TIMES.
                   07  GR               PIC S9(04)    COMP-5.
                   07  QT-FXA           PIC S9(04)    COMP-5.
                   07  LS-FXA OCCURS 8 TIMES.
                       09  FXA          PIC S9(04)    COMP-5.
                       09  AMTR         PIC S9(15)V99 COMP-3.
                       09  PCLD         PIC S9(15)V99 COMP-3.
           03  RPST.
               05  QT-RSTD              PIC S9(04)    COMP-5.
               05  LS-RSTD OCCURS 9 TIMES.
                   07  QT-FXA-RSTD      PIC S9(04)    COMP-5.
                   07  LS-FXA OCCURS 9 TIMES.
                       09  GR-RSTD      PIC S9(04)    COMP-5.
                       09  FXA-RSTD     PIC S9(04)    COMP-5.
                       
       01  PARM.
           03  PRM-GR                   PIC  9(02) VALUE 10.
           03  FILLER                   PIC  X(01).
           03  PRM-FXA                  PIC  9(01) VALUE 8.
       
       PROCEDURE DIVISION.
       000000-PRINCIPAL SECTION.
           DISPLAY CTE-PROG ' Teste'
           MOVE FUNCTION CURRENT-DATE   TO DATA-HORA.
           DISPLAY FUNCTION RANDOM(HORA)
           
           MOVE ZEROS                   TO SEQL-ERRO
           MOVE SPACES                  TO TX-ERRO
           MOVE ZEROS                   TO VL-ENTD
           
           
           COMPUTE QT-GR = FUNCTION RANDOM * (PRM-GR - 1 + 1) + 1
           PERFORM VARYING IC-GR FROM 1 BY 1
           UNTIL IC-GR GREATER QT-GR
               MOVE IC-GR               TO GR(IC-GR)
               COMPUTE QT-FXA(IC-GR) = 
                   FUNCTION RANDOM * (PRM-FXA - 1 + 1) + 1 
               END-COMPUTE
               
               MOVE ZEROS               TO IC-FXA-ANT
               
               PERFORM VARYING IC-FXA FROM 1 BY 1
               UNTIL IC-FXA GREATER QT-FXA(IC-GR)
                   MOVE IC-FXA          TO FXA(IC-GR IC-FXA)
                   
                   IF  IC-FXA EQUAL 1
                       COMPUTE PCLD(IC-GR IC-FXA) = FUNCTION RANDOM * 
                                               (MAX-PCLD - 1 + 1) + 1
                       END-COMPUTE
                       COMPUTE AMTR(IC-GR IC-FXA) =  FUNCTION RANDOM * 
                                     (PCLD(IC-GR IC-FXA) - 1 + 1) + 1
                       END-COMPUTE
                   ELSE
                       COMPUTE PCLD(IC-GR IC-FXA) = FUNCTION RANDOM * 
                                                          (MAX-PCLD - 
                                            PCLD(IC-GR IC-FXA) + 1) + 
                                                 PCLD(IC-GR IC-FXA)
                       END-COMPUTE
                       COMPUTE AMTR(IC-GR IC-FXA) =  FUNCTION RANDOM * 
                                                 (PCLD(IC-GR IC-FXA) - 
                                         AMTR(IC-GR IC-FXA-ANT) + 1) +
                                              AMTR(IC-GR IC-FXA-ANT)
                       END-COMPUTE
                   END-IF
                   
                   MOVE IC-FXA          TO IC-FXA-ANT
               END-PERFORM
               
           END-PERFORM
           
           CALL COMBINA USING BOOK
           
           IF  RETURN-CODE NOT EQUAL ZEROS
               MOVE RETURN-CODE         TO GDA-RETURN-CODE
               DISPLAY CTE-PROG ' RET-CODE: ' GDA-RETURN-CODE
               GO TO 000000-SAI
           END-IF 
           
           IF  SEQL-ERRO NOT EQUAL ZEROS
               DISPLAY CTE-PROG ' (' SEQL-ERRO ') ' TX-ERRO
               GO TO 000000-SAI
           END-IF
           
           DISPLAY CTE-PROG ' 888 - FIM NORMAL'
           .
       000000-SAI.
           STOP RUN.
