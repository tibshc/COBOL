       IDENTIFICATION DIVISION.
       PROGRAM-ID. LaboratorioDia3.
       
       ENVIRONMENT DIVISION.
       input-output section.
       file-control.
           select INPUT-FILE assign to "STUDENTS_1.DAT"
               organization is line sequential
               file status is FILE-CHECK-KEY.
           select ENG-FILE assign to "STUDENTS-ENG.DAT"
               organization is line sequential.
           select BUS-FILE assign to "STUDENTS_BUS.DAT"
               organization is line sequential.
           select SCI-FILE assign to "STUDENTS_SCI.DAT"
               organization is line sequential.
           select IST-FILE assign to "STUDENTS_IST.DAT"
               organization is line sequential.
       
       DATA DIVISION.
       file section.
       
       fd  INPUT-FILE.
       01  STUDENT-INFO.
           88 EOF                  value high-value.
           02 ID-STUDENT           pic 9(7).
           02 LNAME-STUDENT        pic X(10).
           02 FNAME-STUDENT        pic X(10).
           02 DEGCOD-STUDENT       pic X(9).
           02 DEG-STUDENT          pic X(3).
           02 GRADE-STUDENT        pic 9(2)V99.

       fd  ENG-FILE.
       01  ENG-REC                 pic X(64).

       fd  BUS-FILE.
       01  BUS-REC                 pic X(64).

       fd  SCI-FILE.
       01  SCI-REC                 pic X(64).

       fd  IST-FILE.
       01  IST-REC                 pic X(64).

       WORKING-STORAGE SECTION.
      * Variables del programa
           01 WS-WORK-AREAS.
               05 FILE-CHECK-KEY  pic X(2).
           
           01 WS-STUDENT.
               05 WS-REC          pic X(40).
            
           01 WS-AUX.
               05 ENG-AUX.
                   10 ENG-CONT     pic 9(3).
                   10 ENG-SUM      pic 9(4)V99.
                   10 ENG-AVG      pic 99V99.
               05 BUS-AUX.
                   10 BUS-CONT     pic 9(3).
                   10 BUS-SUM      pic 9(4)V99.
                   10 BUS-AVG      pic 99V99.
               05 SCI-AUX.
                   10 SCI-CONT     pic 9(3).
                   10 SCI-SUM      pic 9(4)V99.
                   10 SCI-AVG      pic 99V99.
               05 IST-AUX.
                   10 IST-CONT     pic 9(3).
                   10 IST-SUM      pic 9(4)V99.
                   10 IST-AVG      pic 99V99.

            
           01 WS-REPORT.
               05 RP-TITLE.
                   10 filler       pic X(23).
                   10 filler       pic X(17) value "REPORTE"- "CLASE ".
                   10 RP-CLASS     PIC X(3).
                   10 FILLER       PIC X(24).
               
               05 RP-HEADER1.
                   10 FILLER       PIC X(11)   VALUE "ID".
                   10 FILLER       PIC X(2)    VALUE SPACES.
                   10 FILLER       PIC X(11)   VALUE "APELLIDO".
                   10 FILLER       PIC X(2)    VALUE SPACES.
                   10 FILLER       PIC X(11)   VALUE "NOMBRE".
                   10 FILLER       PIC X(2)    VALUE SPACES.
                   10 FILLER       PIC X(11)   VALUE "CODIGO".
                   10 FILLER       PIC X(2)    VALUE SPACES.
                   10 FILLER       PIC X(2)    VALUE SPACES.
                   10 FILLER       PIC X(2)    VALUE SPACES.
                   10 FILLER       pic X(5)    value "NOTA".


               05 RP-HEADER2.
                   10 filler       pic X(11)   value "ESTUDIANTE".
                   10 filler       pic X(2)    VALUE spaces.
                   10 filler       pic X(11)   value "ESTUDIANTE".
                   10 filler       pic X(2)    VALUE spaces.
                   10 filler       pic X(11)   value "ESTUDIANTE".
                   10 filler       pic X(2)    value spaces.
                   10 filler       pic X(10)   value "CLASE".
                   10 filler       pic X(2)    value spaces.
                   10 filler       pic X(6)    value "CLASE".
                   10 filler       PIC X(2)    value spaces.
                   10 filler       pic X(5)    value "FINAL".

               05  RP-LINE.
                   10  FILLER          PIC X(11)   VALUE ALL "-".
                   10  FILLER          PIC X(2)    VALUE SPACES.
                   10  FILLER          PIC X(11)   VALUE ALL "-".
                   10  FILLER          PIC X(2)    VALUE SPACES.
                   10  FILLER          PIC X(11)   VALUE ALL "-".
                   10  FILLER          PIC X(2)    VALUE SPACES.
                   10  FILLER          PIC X(10)   VALUE ALL "-".
                   10  FILLER          PIC X(2)    VALUE SPACES.
                   10  FILLER          PIC X(6)   VALUE ALL "-".
                   10  FILLER          PIC X(2)    VALUE SPACES.
                   10  FILLER          PIC X(5)   VALUE ALL "-".
               
               05 REP-DET.
                   10 DET-ID           pic X(11).
                   10 filler           pic X(2) value spaces.
                   10 DET-LNAME         pic X(11).
                   10 filler           pic X(2) VALUE SPACES.
                   10 DET-FNAME        PIC X(11).
                   10 FILLER           PIC X(2) VALUE SPACES.
                   10 DET-CODE         PIC X(10).
                   10 FILLER           PIC X(2) VALUE SPACES.
                   10 DET-DEG          PIC X(6).
                   10 FILLER           PIC X(2) VALUE SPACES.
                   10 DET-GRADE        PIC 99.99.
               
               05 RP-DIV               pic X(64)   value all "-".
               
               05 RP-RESUME.
                   10 RS1.
                       15 filler pic X(22)  value "CANTIDAD "-
                                                   "ESTUDAINTES ".
                       15 RS-CONT      pic 999.
                   
                   10 RS2.
                       15 filler       pic X(22) value spaces.
                       15 filler       pic X(10) value "PROMEDIO: ".
                       15 RS-AVG       PIC 99.99.

       PROCEDURE DIVISION.
           perform 0100-INICIO.
           perform 0200-PROCESO.
           perform 0300-FIN.

       0100-INICIO.
           open input INPUT-FILE.
           if FILE-CHECK-KEY not = '00'
               display "ERROR AL ABRIR INPUT FILE"
               display "CODIGO: " FILE-CHECK-KEY
               STOP run
           end-if.
           open output ENG-FILE.
           open output BUS-FILE.
           open output SCI-FILE.
           open output IST-FILE.

          initialize WS-AUX.


       0200-PROCESO.
           perform 0210-READ-INPUT-RECORD.
           if EOF
               display "ARCHIVO SIN REGISTROS"
               perform 0300-FIN.
           perform 0220-WRITE-HEADERS.
           perform 0230-CLASF-RECORDS until EOF.
           perform 0240-WRITE-RESUME.

       0210-READ-INPUT-RECORD.
           read INPUT-FILE
               at end set EOF TO true
           end-read.
       
       0220-WRITE-HEADERS.
           move "ENG" TO RP-CLASS.
           write ENG-REC from RP-TITLE.
           write ENG-REC from RP-HEADER1.
           write ENG-REC from RP-HEADER2.
           write ENG-REC from RP-LINE.

           move "BUS" TO RP-CLASS.
           write BUS-REC from RP-TITLE.
           write BUS-REC from RP-HEADER1.
           write BUS-REC from RP-HEADER2.
           write BUS-REC from RP-LINE.

           move "SCI" TO RP-CLASS.
           write SCI-REC from RP-TITLE.
           write SCI-REC from RP-HEADER1.
           write SCI-REC from RP-HEADER2.
           write SCI-REC from RP-LINE.

           move "IST" TO RP-CLASS.
           write IST-REC from RP-TITLE.
           write IST-REC from RP-HEADER1.
           write IST-REC from RP-HEADER2.
           write IST-REC from RP-LINE.


       0230-CLASF-RECORDS.
           
           move ID-STUDENT to DET-ID.
           MOVE LNAME-STUDENT to DET-LNAME.
           move FNAME-STUDENT TO DET-FNAME.
           move DEGCOD-STUDENT to DET-CODE.
           move DEG-STUDENT TO DET-DEG.
           move GRADE-STUDENT to DET-GRADE.

           evaluate DEG-STUDENT
               when 'ENG'
                   write ENG-REC from REP-DET
                   add 1 to ENG-CONT
                   compute ENG-SUM = ENG-SUM +GRADE-STUDENT
                   compute ENG-AVG = ENG-SUM/ENG-CONT

                when 'BUS'
                   write BUS-REC from REP-DET
                   add 1 to BUS-CONT
                   compute BUS-SUM = BUS-SUM +GRADE-STUDENT
                   compute BUS-AVG = BUS-SUM/BUS-CONT
               
                when 'SCI'
                   write SCI-REC from REP-DET
                   add 1 to SCI-CONT
                   compute SCI-SUM = SCI-SUM +GRADE-STUDENT
                   compute SCI-AVG = SCI-SUM/ENG-CONT
               
                when 'IST'
                   write IST-REC from REP-DET
                   add 1 to IST-CONT
                   compute IST-SUM = IST-sUM + GRADE-STUDENT
                   compute IST-AVG = IST-SUM/ENG-CONT

                when other
                   display "REGISTRO FUERA DE CATEGORIA: " STUDENT-INFO
           end-evaluate.

           perform 0210-READ-INPUT-RECORD.

       0240-WRITE-RESUME.
           MOVE ENG-CONT TO RS-CONT.
           MOVE ENG-AVG TO RS-AVG.
           WRITE ENG-REC FROM RP-DIV.
           WRITE ENG-REC FROM RS1.
           WRITE ENG-REC FROM RS2.

           MOVE BUS-CONT TO RS-CONT
           MOVE BUS-AVG TO RS-AVG
           WRITE BUS-REC FROM RP-DIV.
           WRITE BUS-REC FROM RS1.
           WRITE BUS-REC FROM RS2.

           MOVE SCI-CONT TO RS-CONT
           MOVE SCI-AVG TO RS-AVG
           WRITE SCI-REC FROM RP-DIV.
           WRITE SCI-REC FROM RS1.
           WRITE SCI-REC FROM RS2.

           MOVE IST-CONT TO RS-CONT
           MOVE IST-AVG TO RS-AVG
           WRITE IST-REC FROM RP-DIV.
           WRITE IST-REC FROM RS1.
           WRITE IST-REC FROM RS2.


       0300-FIN.
           CLOSE INPUT-FILE.
           CLOSE ENG-FILE.
           CLOSE BUS-FILE.
           CLOSE SCI-FILE.
           CLOSE IST-FILE.
           STOP RUN.

       PRINCIPAL.
           DISPLAY "Iniciando programa...".
           
           STOP RUN.
       
       END PROGRAM LaboratorioDia3.