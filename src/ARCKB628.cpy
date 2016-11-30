      *----------------------------------------------------------------*
      * BOOK......: ARCKB628.
      * ANALISTA..: F2419497 DIEGO CASAGRANDE
      * AUTOR.....: F2419497 DIEGO CASAGRANDE
      * DATA......: 07.11.2016
      * OBJETIVO..: Book da subrotina ARCSB628.
      *----------------------------------------------------------------*
      * VRS0001 26.10.2016 - F2419497 - Implantacao.
      *----------------------------------------------------------------*

       03  ARCSB628-ERRO.
           05  ARCSB628-SEQL-ERRO                PIC S9(09)    COMP-5.
           05  ARCSB628-TX-ERRO                  PIC X(120).

       03  ARCSB628-RQSC.
           05  ARCSB628-VL-ENTD                  PIC S9(15)V99 COMP-3.
           05  ARCSB628-QT-GR                    PIC S9(04)    COMP-5.
               88  ARCSB628-QT-GR-VLDO           VALUE +1 THRU +10.
           05  ARCSB628-LS-GR            OCCURS 10 TIMES.
               07  ARCSB628-CD-GR-PRD            PIC S9(04)    COMP-5.
               07  ARCSB628-QT-FXA               PIC S9(04)    COMP-5.
                   88  ARCSB628-QT-FXA-VLDO      VALUE +1 THRU +8.
               07  ARCSB628-LS-FXA-RVSA  OCCURS 8 TIMES.
                   09  ARCSB628-CD-FXA-RVSA-RSCO PIC S9(04)    COMP-5.
                   09  ARCSB628-VL-AMTR          PIC S9(15)V99 COMP-3.
                   09  ARCSB628-VL-PCLD          PIC S9(15)V99 COMP-3.

       03  ARCSB628-RPST.
           05  ARCSB628-QT-RSTD                  PIC S9(04)    COMP-5.
               88  ARCSB628-QT-RSTD-VLDO         VALUE +1 THRU +9.
           05  ARCSB628-RSTD OCCURS 9 TIMES.
               07  ARCSB628-QT-FXA-RSTD          PIC S9(04)    COMP-5.
               07  ARCSB628-LS-CBN OCCURS 10 TIMES.
                   09  ARCSB628-CD-GR-PRD-RSTD   PIC S9(04)    COMP-5.
                   09  ARCSB628-CD-FXA-RVSA-RSTD PIC S9(04)    COMP-5.
