       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ACtividad2Dia4.
       AUTHOR.      EHIDALGO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "ARCHIVOS-FISICOS.CBL".
           
       DATA DIVISION.

       FILE SECTION.
       fd ARCHIVO-ESTUDIANTES.
       01 R-ESTUDIANTE.
           88 END-OF-FILE   VALUE HIGH-VALUES.
           02 R-MATRICULA     PIC 9(07).
           02 R-APELLIDO      PIC X(10).
           02 R-NOMBRE        PIC X(10).
           02 R-CODIGO        PIC X(09).
           02 R-MATERIA       PIC X(03).
           02 R-GENERO        PIC X(01).

       fd ARCHIVO-ORDENADO.
       01 R-ORDENADO PIC X(80).

       sd  ARCHIVO-TRABAJO.
       01 REGISTRO-TRABAJO.
           02 NUMERO-MATRICULA         pic 9(7).
           02 APELLIDO                 pic X(10).
           02 NOMBRE                   PIC X(10).   
           02 CODIGO                   pic X(9).
           02 MATERIA                  pic X(3).
           02 GENERO                   pic X(1).

           
       WORKING-STORAGE SECTION.
      
       01  ESTADO-ARCHIVO              PIC X(02).
       01  CONT-TOTAL-REGISTROS        pic 9(5) value zero.

       01 W-REGISTRO-BONITO.
          05 O-MATRICULA       PIC 9(7).
          05 S1                PIC X(3) VALUE " | ".
          05 O-APELLIDO        PIC X(10).
          05 S2                PIC X(1) VALUE " ".
          05 O-NOMBRE          PIC X(10).
          05 S3                PIC X(3) VALUE " | ".
          05 O-CODIGO          PIC X(9).
          05 S4                PIC X(3) VALUE " | ".
          05 O-MATERIA         PIC X(3).
          05 S5                PIC X(3) VALUE " | ".
          05 O-GENERO           PIC X(1).

       01 W-EOF-SORT           pic X(1) value "N".
           88 NO-HAY-MAS-SORT  value "S".

       PROCEDURE DIVISION.
       MAIN. 
           DISPLAY "INICIO DEL PROGRAMA".
           display "----------------------------------".
           OPEN OUTPUT ARCHIVO-ORDENADO.

           sort ARCHIVO-TRABAJO
               ON ASCENDING KEY MATERIA, APELLIDO, NOMBRE
               USING ARCHIVO-ESTUDIANTES
               OUTPUT PROCEDURE IS GENERAR-SALIDA.
           CLOSE ARCHIVO-ORDENADO.
           STOP RUN.

       GENERAR-SALIDA.
           return ARCHIVO-TRABAJO at END SET NO-HAY-MAS-SORT TO TRUE.
           perform until NO-HAY-MAS-SORT
           move NUMERO-MATRICULA to O-MATRICULA
           move APELLIDO TO O-APELLIDO
           move NOMBRE TO O-NOMBRE
           move CODIGO TO O-CODIGO
           move MATERIA TO O-MATERIA
           move GENERO TO O-GENERO

           MOVE " | " TO S1 S3 S4 S5
           MOVE " "   TO S2

        
           write R-ORDENADO from W-REGISTRO-BONITO
           call "VALIDAR-ESTUDIANTE" using REGISTRO-TRABAJO
         
           compute  CONT-TOTAL-REGISTROS = CONT-TOTAL-REGISTROS + 1
           
           return ARCHIVO-TRABAJO AT END SET NO-HAY-MAS-SORT TO true
           end-return
          
           end-perform.

       END PROGRAM ACtividad2Dia4.
