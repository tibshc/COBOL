       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PROCESAR-ESTUDIANTE.
       AUTHOR.      EHIDALGO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NOTA                     PIC ZZ9,99.

       LINKAGE SECTION.
       01 R-REGISTRO-ESTUDIANTE.
          02 R-NUMERO-MATRICULA       PIC 9(7).
          02 R-APELLIDO               PIC X(10).
          02 R-NOMBRE                 PIC X(10).
          02 R-CARRERA                PIC X(3).
          02 R-GENERO                 PIC X(1).
          02 R-NOTA                   PIC 9(2)V9(2).
       
       01 L-TOTAL-NOTA-ENG PIC 9(7)V9(2).
       01 L-TOTAL-NOTA-BUS PIC 9(7)V9(2).
       01 L-TOTAL-NOTA-SCI PIC 9(7)V9(2).
       01 L-TOTAL-NOTA-CPS PIC 9(7)V9(2).
       01 L-CONTADOR-ENG PIC 9(5).
       01 L-CONTADOR-BUS PIC 9(5).
       01 L-CONTADOR-SCI PIC 9(5).
       01 L-CONTADOR-CPS PIC 9(5).

       PROCEDURE DIVISION USING R-REGISTRO-ESTUDIANTE
                                L-TOTAL-NOTA-ENG
                                L-TOTAL-NOTA-BUS
                                L-TOTAL-NOTA-SCI
                                L-TOTAL-NOTA-CPS
                                L-CONTADOR-ENG
                                L-CONTADOR-BUS
                                L-CONTADOR-SCI
                                L-CONTADOR-CPS.

            MOVE R-NOTA TO WS-NOTA.

            DISPLAY "ESTUDIANTE: " R-NOMBRE " " R-APELLIDO
               " - CARRERA: " R-CARRERA
               " - NOTA: " WS-NOTA.

           EVALUATE R-CARRERA
               WHEN "ENG"
                    ADD R-NOTA TO L-TOTAL-NOTA-ENG
                    ADD 1 TO L-CONTADOR-ENG
               WHEN "BUS"
                    ADD R-NOTA TO L-TOTAL-NOTA-BUS
                    ADD 1 TO L-CONTADOR-BUS
               WHEN "SCI"
                    ADD R-NOTA TO L-TOTAL-NOTA-SCI
                    ADD 1 TO L-CONTADOR-SCI
               WHEN "CPS"
                    ADD R-NOTA TO L-TOTAL-NOTA-CPS
                    ADD 1 TO L-CONTADOR-CPS
               WHEN OTHER
                    DISPLAY "CARRERA NO RECONOCIDA PARA: " R-NOMBRE " "
                     R-APELLIDO 
                    " - CARRERA: " R-CARRERA
               END-EVALUATE.

       END PROGRAM PROCESAR-ESTUDIANTE.