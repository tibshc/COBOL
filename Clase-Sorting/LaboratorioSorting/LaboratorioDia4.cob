       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LaboratorioDia4.
       AUTHOR.      EHIDALGO.
       DATE-WRITTEN. 21/04/2026.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-ENTRADA ASSIGN TO "LABORATORY.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-ORDENADO ASSIGN TO "ORDENADOS.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-TRABAJO ASSIGN TO "TRABAJO.TMP".

       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-ENTRADA.
       01 R-REGISTRO-ESTUDIANTE.
          88 END-OF-FILE VALUE HIGH-VALUES.
          02 R-NUMERO-MATRICULA       PIC 9(7).
          02 R-APELLIDO               PIC X(10).
          02 R-NOMBRE                 PIC X(10).
          02 R-CARRERA                PIC X(3).
          02 R-GENERO                 PIC X(1).
          02 R-NOTA                   PIC 9(2)V9(2).

       FD ARCHIVO-ORDENADO.
       01 R-ORDENADO PIC X(80).


       SD ARCHIVO-TRABAJO.
       01 REGISTRO-TRABAJO.
          02 NUMERO-MATRICULA       PIC 9(7).
          02 APELLIDO               PIC X(10).
          02 NOMBRE                 PIC X(10).
          02 CARRERA                PIC X(3).
          02 GENERO                 PIC X(1).
          02 NOTA                   PIC 9(2)V9(2).

       WORKING-STORAGE SECTION.
       
       01 TOTAL-ESTUDIANTES          PIC 9(5) VALUE ZERO.
       01 TOTAL-NOTA-ENG             PIC 9(7)V9(2) VALUE ZERO.
       01 TOTAL-NOTA-BUS             PIC 9(7)V9(2) VALUE ZERO.
       01 TOTAL-NOTA-SCI             PIC 9(7)V9(2) VALUE ZERO.
       01 TOTAL-NOTA-CPS             PIC 9(7)V9(2) VALUE ZERO.
       01 CONTADOR-ENG               PIC 9(5) VALUE ZERO.
       01 CONTADOR-BUS               PIC 9(5) VALUE ZERO.
       01 CONTADOR-SCI               PIC 9(5) VALUE ZERO.
       01 CONTADOR-CPS               PIC 9(5) VALUE ZERO.
       
       01 W-REGISTRO-BONITO.
          05 O-MATRICULA       PIC 9(7).
          05 S1                PIC X(3) VALUE " | ".
          05 O-APELLIDO        PIC X(10).
          05 S2                PIC X(1) VALUE " ".
          05 O-NOMBRE          PIC X(10).
          05 S3                PIC X(3) VALUE " | ".
          05 O-CARRERA         PIC X(3).
          05 S4                PIC X(3) VALUE " | ".
          05 O-GENERO          PIC X(1).
          05 S5                PIC X(3) VALUE " | ".
          05 O-NOTA            PIC ZZ9.99.

       01 W-HEADER-1.
          05 FILLER PIC X(80) VALUE 
          "MATRICUL | APELLIDO   NOMBRE     | CAR | G |  NOTA ".
       01 W-HEADER-2.
          05 FILLER PIC X(80) VALUE 
          "---------|-----------------------|-----|---|-------".

       01 W-EOF-SORT                 PIC X(1) VALUE "N".
          88 NO-HAY-MAS-SORT VALUE "S".

       PROCEDURE DIVISION.
      
       PRINCIPAL.
           DISPLAY "INICIANDO PROCESO DE ORDENAMIENTO...".
           
           OPEN OUTPUT ARCHIVO-ORDENADO.
           
           WRITE R-ORDENADO FROM W-HEADER-1.
           WRITE R-ORDENADO FROM W-HEADER-2.

           SORT ARCHIVO-TRABAJO 
             ON ASCENDING KEY CARRERA, NOTA, APELLIDO
             USING ARCHIVO-ENTRADA
             OUTPUT PROCEDURE IS 200-GENERAR-SALIDA.

           CLOSE ARCHIVO-ORDENADO.
           DISPLAY "PROCESO TERMINADO CON EXITO.".
           STOP RUN.

       200-GENERAR-SALIDA.
           RETURN ARCHIVO-TRABAJO AT END SET NO-HAY-MAS-SORT TO TRUE.
           PERFORM UNTIL NO-HAY-MAS-SORT
              MOVE NUMERO-MATRICULA TO O-MATRICULA
              MOVE APELLIDO         TO O-APELLIDO
              MOVE NOMBRE           TO O-NOMBRE
              MOVE CARRERA          TO O-CARRERA
              MOVE GENERO           TO O-GENERO
              MOVE NOTA             TO O-NOTA
              
              MOVE " | " TO S1 S3 S4 S5
              MOVE " "   TO S2
              
              WRITE R-ORDENADO FROM W-REGISTRO-BONITO
              
              CALL 'PROCESAR-ESTUDIANTE' USING REGISTRO-TRABAJO
                 TOTAL-NOTA-ENG TOTAL-NOTA-BUS TOTAL-NOTA-SCI
                 TOTAL-NOTA-CPS CONTADOR-ENG CONTADOR-BUS
                 CONTADOR-SCI CONTADOR-CPS
                 
              RETURN ARCHIVO-TRABAJO AT END SET NO-HAY-MAS-SORT TO TRUE
           END-PERFORM.

       END PROGRAM LaboratorioDia4.
