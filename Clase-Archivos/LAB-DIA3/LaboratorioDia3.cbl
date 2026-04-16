       IDENTIFICATION DIVISION.
       PROGRAM-ID.     LaboratorioDia3.
       AUTHOR.         J R.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "STUDENTS_1.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-CHECK-KEY.
           SELECT ENG-FILE ASSIGN TO "STUDENTS_ENG.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BUS-FILE ASSIGN TO "STUDENTS_BUS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SCI-FILE ASSIGN TO "STUDENTS_SCI.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT IST-FILE ASSIGN TO "STUDENTS_IST.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  STUDENT-INFO.
           88  EOF                 VALUE HIGH-VALUE.
           02  ID-STUDENT          PIC 9(7).
           02  LNAME-STUDENT       PIC X(10).
           02  FNAME-STUDENT       PIC X(10).
           02  DEGCOD-STUDENT      PIC X(9).
           02  DEG-STUDENT         PIC X(3).
           02  GRADE-STUDENT       PIC 9(2)V99.

       FD  ENG-FILE.
       01  ENG-REC                  PIC X(64).

       FD  BUS-FILE.
       01  BUS-REC                  PIC X(64).

       FD  SCI-FILE.
       01  SCI-REC                  PIC X(64).

       FD  IST-FILE.
       01  IST-REC                  PIC X(64).

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREAS.
           05  FILE-CHECK-KEY      PIC X(2).
       01  WS-STUDENT.
           05  WS-REC              PIC X(40).

       01  WS-AUX.
           05  ENG-AUX.
               10  ENG-CONT        PIC 9(3).
               10  ENG-SUM         PIC 9(4)V99.
               10  ENG-AVG         PIC 99V99.
           05  BUS-AUX.
               10  BUS-CONT        PIC 9(3).
               10  BUS-SUM         PIC 9(4)V99.
               10  BUS-AVG         PIC 99V99.
           05  SCI-AUX.
               10  SCI-CONT        PIC 9(3).
               10  SCI-SUM         PIC 9(4)V99.
               10  SCI-AVG         PIC 99V99.
           05  IST-AUX.
               10  IST-CONT        PIC 9(3).
               10  IST-SUM         PIC 9(4)V99.
               10  IST-AVG         PIC 99V99.



       01  WS-REPORT.
           05  RP-TITLE.
               10  FILLER          PIC X(23).
               10  FILLER          PIC X(17)   VALUE "REPORTE "-
                                       "CLASE ".
               10  RP-CLASS        PIC X(3).
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
               10  FILLER          PIC X(6)   VALUE SPACES.
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(5)   VALUE "NOTA".

           05  RP-HEADER2.
               10  FILLER          PIC X(11)   VALUE "ESTUDIANTE".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(11)   VALUE "ESTUDIANTE".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(11)   VALUE "ESTUDIANTE".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(10)   VALUE "CLASE".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(6)   VALUE "CLASE".
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  FILLER          PIC X(5)   VALUE "FINAL".

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

           05  REP-DET.
               10  DET-ID          PIC X(11).
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  DET-LNAME       PIC X(11).
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  DET-FNAME       PIC X(11).
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  DET-CODE        PIC X(10).
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  DET-DEG         PIC X(6).
               10  FILLER          PIC X(2)    VALUE SPACES.
               10  DET-GRADE       PIC 99.99.

           05  RP-DIV              PIC X(64)   VALUE ALL "-".

           05  RP-RESUME.
               10  RS1.
                   15  FILLER      PIC X(22)   VALUE "Cantidad "-
                                       "Estudiantes: ".
                   15  RS-CONT     PIC 999.
               10  RS2.
                   15  FILLER      PIC X(12) VALUE SPACES.
                   15  FILLER      PIC X(10) VALUE "Promedio: ".
                   15  RS-AVG      PIC 99.99.


       PROCEDURE DIVISION.
           PERFORM 0100-INICIO.
           PERFORM 0200-PROCESO.
           PERFORM 0300-FIN.

       0100-INICIO.
           OPEN INPUT INPUT-FILE.
           IF FILE-CHECK-KEY NOT = '00'
               DISPLAY 'Error al abrir INPUT-FILE'
               DISPLAY 'CODIGO: ' FILE-CHECK-KEY
               STOP RUN
           END-IF.
           OPEN OUTPUT ENG-FILE.
           OPEN OUTPUT BUS-FILE.
           OPEN OUTPUT SCI-FILE.
           OPEN OUTPUT IST-FILE.

           INITIALIZE WS-AUX.

       0200-PROCESO.
           PERFORM 0210-READ-INPUT-RECORD.
           IF EOF
               DISPLAY 'Archivo sin registros'
               PERFORM 0300-FIN.
           PERFORM 0220-WRITE-HEADERS.
           PERFORM 0230-CLASF-RECORDS UNTIL EOF.
           PERFORM 0240-WRITE-RESUME.

       0210-READ-INPUT-RECORD.
           READ INPUT-FILE
               AT END SET EOF TO TRUE
           END-READ.

       0220-WRITE-HEADERS.
           MOVE "ENG" TO RP-CLASS.
           WRITE ENG-REC FROM RP-TITLE.
           WRITE ENG-REC FROM RP-HEADER1.
           WRITE ENG-REC FROM RP-HEADER2.
           WRITE ENG-REC FROM RP-LINE.

           MOVE "BUS" TO RP-CLASS.
           WRITE BUS-REC FROM RP-TITLE.
           WRITE BUS-REC FROM RP-HEADER1.
           WRITE BUS-REC FROM RP-HEADER2.
           WRITE BUS-REC FROM RP-LINE.

           MOVE "SCI" TO RP-CLASS.
           WRITE SCI-REC FROM RP-TITLE.
           WRITE SCI-REC FROM RP-HEADER1.
           WRITE SCI-REC FROM RP-HEADER2.
           WRITE SCI-REC FROM RP-LINE.

           MOVE "IST" TO RP-CLASS.
           WRITE IST-REC FROM RP-TITLE.
           WRITE IST-REC FROM RP-HEADER1.
           WRITE IST-REC FROM RP-HEADER2.
           WRITE IST-REC FROM RP-LINE.

       0230-CLASF-RECORDS.

           MOVE ID-STUDENT TO DET-ID.
           MOVE LNAME-STUDENT TO DET-LNAME.
           MOVE FNAME-STUDENT TO DET-FNAME.
           MOVE DEGCOD-STUDENT TO DET-CODE.
           MOVE DEG-STUDENT TO DET-DEG.
           MOVE GRADE-STUDENT TO DET-GRADE.

           EVALUATE DEG-STUDENT
               WHEN 'ENG'
                   WRITE ENG-REC FROM REP-DET
                   ADD 1 TO ENG-CONT
                   COMPUTE ENG-SUM = ENG-SUM + GRADE-STUDENT
                   COMPUTE ENG-AVG = ENG-SUM / ENG-CONT

               WHEN 'BUS'
                   WRITE BUS-REC FROM REP-DET
                   ADD 1 TO BUS-CONT
                   COMPUTE BUS-SUM = BUS-SUM + GRADE-STUDENT
                   COMPUTE BUS-AVG = BUS-SUM / BUS-CONT

               WHEN 'SCI'
                   WRITE SCI-REC FROM REP-DET
                   ADD 1 TO SCI-CONT
                   COMPUTE SCI-SUM = SCI-SUM + GRADE-STUDENT
                   COMPUTE SCI-AVG = SCI-SUM / SCI-CONT

               WHEN 'IST'
                   WRITE IST-REC FROM REP-DET
                   ADD 1 TO IST-CONT
                   COMPUTE IST-SUM = IST-SUM + GRADE-STUDENT
                   COMPUTE IST-AVG = IST-SUM / IST-CONT

               WHEN OTHER
                   DISPLAY 'REGISTRO FUERA DE CATEGORIA: ' STUDENT-INFO
           END-EVALUATE.

           PERFORM 0210-READ-INPUT-RECORD.

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

       END PROGRAM LaboratorioDia3.
