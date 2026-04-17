       IDENTIFICATION DIVISION.
       PROGRAM-ID. ActividadDia3.
       author.      tibs.

       ENVIRONMENT DIVISION.
       input-output section.
       file-control.
           select INPUT-FILE assign to "STUDENTS_1.DAT"
               organization is line sequential
               file status is FILE-CHECK-KEY.
           
           select M-FILE assign to "STUDENTS_MG.DAT"
               organization is line sequential.

           select W-FILE assign to "STUDENTS_FG_DAT"
               organization  is line sequential.

       DATA DIVISION.
       FILE SECTION.
       fd  INPUT-FILE.
       01 STUDENT-INFO.
           88 EOF                      value high-value.
           02 ID-STUDENT               pic 9(7).
           02 APELL-STUDENT            pic X(10).
           02 NOMBRE-STUDENT           pic X(10).
           02 CODGR-STUDENT            pic X(9).
           02 GRAD-STUDENT             pic X(3).
           02 GEN-STUDENT              pic X.

       fd  M-FILE.
       01  M-REC                       pic X(64).

       fd  W-FILE.
       01  W-REC                       pic X(64).


       WORKING-STORAGE SECTION.
       
       01  WS-WORK-AREAS.
           05 FILE-CHECK-KEY           pic X(2).
       01  WS-STUDENT.
           05 WS-REC                   pic X(40).

       01  WS-REPORT.
           05  RP-TITLE.
               10  FILLER          PIC X(23).
               10  FILLER          PIC X(17)   VALUE "REPORTE ".
              
               10  RP-GEN          PIC X(3).
               10  FILLER          PIC X(24).

           05  RP-HEADER1.
               10  FILLER          PIC X(11)   VALUE "ID".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(11)   VALUE "APELLIDO".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(11)   VALUE "NOMBRE".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(10)   VALUE "CODIGO".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(6)    VALUE   "CLASE".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(6)    VALUE "GENERO".


           05  RP-LINE.
               10  FILLER          PIC X(11)   VALUE ALL "-".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(11)   VALUE ALL "-".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(11)   VALUE ALL "-".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(10)   VALUE ALL "-".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(6)    VALUE ALL "-".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(5)    VALUE ALL "-".

            05  REP-DET.
               10  DET-ID          PIC X(11).
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  DET-APELL      PIC X(11).
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  DET-NOMBRE      PIC X(11).
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  DET-COD        PIC X(10).
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  DET-GRAD         PIC X(6).
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  DET-GEN         PIC X.
           
           05 RP-DIV               pic X(64)   value all "-".

       
       PROCEDURE DIVISION.

           perform 0100-INICIO.
           perform 0200-PROCESO.
           perform 0300-FIN.

       0100-INICIO.
           OPEN INPUT INPUT-FILE.
           IF FILE-CHECK-KEY NOT = '00'
                  DISPLAY "ERROR AL ABRIR ARCHIVO"
                  DISPLAY "CODIGO: " FILE-CHECK-KEY
                  STOP RUN
           END-IF. 
       
           OPEN output M-FILE.
           open output W-FILE.

       0200-PROCESO.
           perform 0210-READ-INPUT-RECORD.
           if EOF
               display "ARCHIVO SIN REGISTROS"
               perform 0300-FIN.
           perform 0220-WRITE-HEADERS.
           perform 0230-CLASF-RECORDS until EOF.
       

       0210-READ-INPUT-RECORD.
           READ INPUT-FILE
               AT END SET EOF TO TRUE
           END-READ.

       0220-WRITE-HEADERS.
           MOVE "M" TO RP-GEN.
           WRITE M-REC FROM RP-TITLE.
           WRITE M-REC FROM RP-HEADER1
           WRITE M-REC FROM RP-LINE.    

           MOVE "W" to RP-GEN.
           write W-REC from RP-TITLE.
           write W-REC from RP-HEADER1.
           write W-REC from RP-LINE.

       0230-CLASF-RECORDS.
           
           move ID-STUDENT TO DET-ID.
           move APELL-STUDENT TO DET-APELL.
           move NOMBRE-STUDENT TO DET-NOMBRE.
           move CODGR-STUDENT TO DET-COD.
           move GRAD-STUDENT TO DET-GRAD.
           MOVE GEN-STUDENT TO DET-GEN.

           evaluate GEN-STUDENT
               when "M"
                   write M-REC from REP-DET
               when "F"
                   write W-REC from REP-DET
               when other
               display "REGISTRO FUERA DE CATEGORIA: " NOMBRE-STUDENT
               APELL-STUDENT
               GEN-STUDENT
           end-evaluate.

           perform 0210-READ-INPUT-RECORD.

       0300-FIN.
           close INPUT-FILE.
           close M-FILE.
           close W-FILE.
           stop run.

               
       END PROGRAM ActividadDia3.