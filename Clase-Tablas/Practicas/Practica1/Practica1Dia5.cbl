       IDENTIFICATION DIVISION.
       PROGRAM-ID. PracticaDia5.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "STUDENTS_5.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-CHECK-KEY.

           SELECT ENG-FILE ASSIGN TO "REPORT_ENG.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT BUS-FILE ASSIGN TO "REPORT_BUS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT IST-FILE ASSIGN TO "REPORT_IST.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SCI-FILE ASSIGN TO "REPORT_SCI.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  INPUT-FILE.
       01  STUDENT-INFO.
           88  EOF VALUE HIGH-VALUE.
           02  STUDENT-ID       PIC 9(7).
           02  STUDENT-LNAME    PIC X(10).
           02  STUDENT-FNAME    PIC X(10).
           02  STUDENT-DEGCOD   PIC X(9).
           02  STUDENT-DEG      PIC X(3).
           02  STUDENT-GRADE    PIC 9(2)V99 OCCURS 3 TIMES.

       FD  ENG-FILE.
       01  ENG-LN               PIC X(80).

       FD  BUS-FILE.
       01  BUS-LN               PIC X(80).

       FD  IST-FILE.
       01  IST-LN               PIC X(80).

       FD  SCI-FILE.
       01  SCI-LN               PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREAS.
           05  FILE-CHECK-KEY   PIC X(2).
           05  ERR-MSG          PIC X(128).
           05  ERR-CODE         PIC X(2).
           05  WS-SUBSCRIPT     PIC 99.

       01  RP-FORMAT.
           05  RP-TITLE.
               10  FILLER       PIC X(20) VALUE SPACES.
               10  FILLER       PIC X(15) VALUE "REPORTE CLASE: ".
               10  RP-CLASS-NAME PIC X(3).
               10  FILLER       PIC X(42) VALUE SPACES.

           05  RP-HEADER.
               10  FILLER       PIC X(7)  VALUE "ID".
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  FILLER       PIC X(10) VALUE "NOMBRE".
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  FILLER       PIC X(10) VALUE "APELLIDO".
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  FILLER       PIC X(5)  VALUE "N1".
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  FILLER       PIC X(5)  VALUE "N2".
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  FILLER       PIC X(5)  VALUE "N3".
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  FILLER       PIC X(5)  VALUE "AVG".
               10  FILLER       PIC X(21) VALUE SPACES.

           05  RP-SEP-LINE.
               10  FILLER       PIC X(7)  VALUE ALL "-".
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  FILLER       PIC X(10) VALUE ALL "-".
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  FILLER       PIC X(10) VALUE ALL "-".
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  FILLER       PIC X(5)  VALUE ALL "-".
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  FILLER       PIC X(5)  VALUE ALL "-".
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  FILLER       PIC X(5)  VALUE ALL "-".
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  FILLER       PIC X(5)  VALUE ALL "-".
               10  FILLER       PIC X(21) VALUE SPACES.

           05  RP-DET-LINE.
               10  RP-ID        PIC 9(7).
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  RP-FNAME     PIC X(10).
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  RP-LNAME     PIC X(10).
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  RP-N1        PIC 9(2)V99.
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  RP-N2        PIC 9(2)V99.
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  RP-N3        PIC 9(2)V99.
               10  FILLER       PIC X(2)  VALUE SPACES.
               10  RP-AVG       PIC 9(2)V99.
               10  FILLER       PIC X(21) VALUE SPACES.

       01  WS-ENG-CLASS.
           05  WS-ENG-STUDENT OCCURS 10 TIMES INDEXED BY ENG-IDX.
               10  WS-ENG-ID       PIC 9(7).
               10  WS-ENG-LNAME    PIC X(10).
               10  WS-ENG-FNAME    PIC X(10).
               10  WS-ENG-DEGCOD   PIC X(9).
               10  WS-ENG-DEG      PIC X(3).
               10  WS-ENG-GRADES.
                   15  WS-ENG-GRADE PIC 9(2)V99 OCCURS 4 TIMES.

       01  WS-BUS-CLASS.
           05  WS-BUS-STUDENT OCCURS 10 TIMES INDEXED BY BUS-IDX.
               10  WS-BUS-ID       PIC 9(7).
               10  WS-BUS-LNAME    PIC X(10).
               10  WS-BUS-FNAME    PIC X(10).
               10  WS-BUS-DEGCOD   PIC X(9).
               10  WS-BUS-DEG      PIC X(3).
               10  WS-BUS-GRADES.
                   15  WS-BUS-GRADE PIC 9(2)V99 OCCURS 4 TIMES.

       01  WS-IST-CLASS.
           05  WS-IST-STUDENT OCCURS 10 TIMES INDEXED BY IST-IDX.
               10  WS-IST-ID       PIC 9(7).
               10  WS-IST-LNAME    PIC X(10).
               10  WS-IST-FNAME    PIC X(10).
               10  WS-IST-DEGCOD   PIC X(9).
               10  WS-IST-DEG      PIC X(3).
               10  WS-IST-GRADES.
                   15  WS-IST-GRADE PIC 9(2)V99 OCCURS 4 TIMES.

       01  WS-SCI-CLASS.
           05  WS-SCI-STUDENT OCCURS 10 TIMES INDEXED BY SCI-IDX.
               10  WS-SCI-ID       PIC 9(7).
               10  WS-SCI-LNAME    PIC X(10).
               10  WS-SCI-FNAME    PIC X(10).
               10  WS-SCI-DEGCOD   PIC X(9).
               10  WS-SCI-DEG      PIC X(3).
               10  WS-SCI-GRADES.
                   15  WS-SCI-GRADE PIC 9(2)V99 OCCURS 4 TIMES.

       PROCEDURE DIVISION.
           PERFORM 0100-INICIO.
           PERFORM 0200-PROCESO.
           PERFORM 0300-FIN.

       0100-INICIO.
           INITIALIZE WS-WORK-AREAS.
           SET ENG-IDX TO 1.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT ENG-FILE BUS-FILE IST-FILE SCI-FILE.
           IF FILE-CHECK-KEY NOT = '00'
               MOVE 'Error al abrir INPUT-FILE. CODIGO: ' TO ERR-MSG
               MOVE FILE-CHECK-KEY TO ERR-CODE
               PERFORM 0310-ERR-FIN
           END-IF.
           PERFORM 0150-WRITE-HEADERS.

       0150-WRITE-HEADERS.
           MOVE "ENG" TO RP-CLASS-NAME.
           WRITE ENG-LN FROM RP-TITLE.
           WRITE ENG-LN FROM RP-HEADER.
           WRITE ENG-LN FROM RP-SEP-LINE.

           MOVE "BUS" TO RP-CLASS-NAME.
           WRITE BUS-LN FROM RP-TITLE.
           WRITE BUS-LN FROM RP-HEADER.
           WRITE BUS-LN FROM RP-SEP-LINE.

           MOVE "IST" TO RP-CLASS-NAME.
           WRITE IST-LN FROM RP-TITLE.
           WRITE IST-LN FROM RP-HEADER.
           WRITE IST-LN FROM RP-SEP-LINE.

           MOVE "SCI" TO RP-CLASS-NAME.
           WRITE SCI-LN FROM RP-TITLE.
           WRITE SCI-LN FROM RP-HEADER.
           WRITE SCI-LN FROM RP-SEP-LINE.

       0200-PROCESO.
           PERFORM UNTIL EOF
               PERFORM 0210-READ-FILE
               EVALUATE STUDENT-DEG
                   WHEN "ENG"
                       MOVE STUDENT-ID     TO WS-ENG-ID(ENG-IDX)
                       MOVE STUDENT-LNAME  TO WS-ENG-LNAME(ENG-IDX)
                       MOVE STUDENT-FNAME  TO WS-ENG-FNAME(ENG-IDX)
                       MOVE STUDENT-DEGCOD TO WS-ENG-DEGCOD(ENG-IDX)
                       MOVE STUDENT-DEG    TO WS-ENG-DEG(ENG-IDX)
                       PERFORM VARYING WS-SUBSCRIPT FROM 1 BY 1
                               UNTIL WS-SUBSCRIPT > 3
                           MOVE STUDENT-GRADE(WS-SUBSCRIPT) TO
                                WS-ENG-GRADE(ENG-IDX,WS-SUBSCRIPT)
                       END-PERFORM
                       CALL "practicaDia5sub"
                           USING WS-ENG-GRADES(ENG-IDX)
                       MOVE WS-ENG-ID(ENG-IDX)      TO RP-ID
                       MOVE WS-ENG-FNAME(ENG-IDX)   TO RP-FNAME
                       MOVE WS-ENG-LNAME(ENG-IDX)   TO RP-LNAME
                       MOVE WS-ENG-GRADE(ENG-IDX,1) TO RP-N1
                       MOVE WS-ENG-GRADE(ENG-IDX,2) TO RP-N2
                       MOVE WS-ENG-GRADE(ENG-IDX,3) TO RP-N3
                       MOVE WS-ENG-GRADE(ENG-IDX,4) TO RP-AVG
                       WRITE ENG-LN FROM RP-DET-LINE
                       SET ENG-IDX UP BY 1
                   WHEN "BUS"
                       MOVE STUDENT-ID     TO WS-BUS-ID(BUS-IDX)
                       MOVE STUDENT-LNAME  TO WS-BUS-LNAME(BUS-IDX)
                       MOVE STUDENT-FNAME  TO WS-BUS-FNAME(BUS-IDX)
                       MOVE STUDENT-DEGCOD TO WS-BUS-DEGCOD(BUS-IDX)
                       MOVE STUDENT-DEG    TO WS-BUS-DEG(BUS-IDX)
                       PERFORM VARYING WS-SUBSCRIPT FROM 1 BY 1
                               UNTIL WS-SUBSCRIPT > 3
                           MOVE STUDENT-GRADE(WS-SUBSCRIPT) TO
                                WS-BUS-GRADE(BUS-IDX,WS-SUBSCRIPT)
                       END-PERFORM
                       CALL "practicaDia5sub"
                           USING WS-BUS-GRADES(BUS-IDX)
                       MOVE WS-BUS-ID(BUS-IDX)      TO RP-ID
                       MOVE WS-BUS-FNAME(BUS-IDX)   TO RP-FNAME
                       MOVE WS-BUS-LNAME(BUS-IDX)   TO RP-LNAME
                       MOVE WS-BUS-GRADE(BUS-IDX,1) TO RP-N1
                       MOVE WS-BUS-GRADE(BUS-IDX,2) TO RP-N2
                       MOVE WS-BUS-GRADE(BUS-IDX,3) TO RP-N3
                       MOVE WS-BUS-GRADE(BUS-IDX,4) TO RP-AVG
                       WRITE BUS-LN FROM RP-DET-LINE
                       SET BUS-IDX UP BY 1
                   WHEN "IST"
                       MOVE STUDENT-ID     TO WS-IST-ID(IST-IDX)
                       MOVE STUDENT-LNAME  TO WS-IST-LNAME(IST-IDX)
                       MOVE STUDENT-FNAME  TO WS-IST-FNAME(IST-IDX)
                       MOVE STUDENT-DEGCOD TO WS-IST-DEGCOD(IST-IDX)
                       MOVE STUDENT-DEG    TO WS-IST-DEG(IST-IDX)
                       PERFORM VARYING WS-SUBSCRIPT FROM 1 BY 1
                               UNTIL WS-SUBSCRIPT > 3
                           MOVE STUDENT-GRADE(WS-SUBSCRIPT) TO
                                WS-IST-GRADE(IST-IDX,WS-SUBSCRIPT)
                       END-PERFORM
                       CALL "practicaDia5sub"
                           USING WS-IST-GRADES(IST-IDX)
                       MOVE WS-IST-ID(IST-IDX)      TO RP-ID
                       MOVE WS-IST-FNAME(IST-IDX)   TO RP-FNAME
                       MOVE WS-IST-LNAME(IST-IDX)   TO RP-LNAME
                       MOVE WS-IST-GRADE(IST-IDX,1) TO RP-N1
                       MOVE WS-IST-GRADE(IST-IDX,2) TO RP-N2
                       MOVE WS-IST-GRADE(IST-IDX,3) TO RP-N3
                       MOVE WS-IST-GRADE(IST-IDX,4) TO RP-AVG
                       WRITE IST-LN FROM RP-DET-LINE
                       SET IST-IDX UP BY 1
                   WHEN "SCI"
                       MOVE STUDENT-ID     TO WS-SCI-ID(SCI-IDX)
                       MOVE STUDENT-LNAME  TO WS-SCI-LNAME(SCI-IDX)
                       MOVE STUDENT-FNAME  TO WS-SCI-FNAME(SCI-IDX)
                       MOVE STUDENT-DEGCOD TO WS-SCI-DEGCOD(SCI-IDX)
                       MOVE STUDENT-DEG    TO WS-SCI-DEG(SCI-IDX)
                       PERFORM VARYING WS-SUBSCRIPT FROM 1 BY 1
                               UNTIL WS-SUBSCRIPT > 3
                           MOVE STUDENT-GRADE(WS-SUBSCRIPT) TO
                                WS-SCI-GRADE(SCI-IDX,WS-SUBSCRIPT)
                       END-PERFORM
                       CALL "practicaDia5sub"
                           USING WS-SCI-GRADES(SCI-IDX)
                       MOVE WS-SCI-ID(SCI-IDX)      TO RP-ID
                       MOVE WS-SCI-FNAME(SCI-IDX)   TO RP-FNAME
                       MOVE WS-SCI-LNAME(SCI-IDX)   TO RP-LNAME
                       MOVE WS-SCI-GRADE(SCI-IDX,1) TO RP-N1
                       MOVE WS-SCI-GRADE(SCI-IDX,2) TO RP-N2
                       MOVE WS-SCI-GRADE(SCI-IDX,3) TO RP-N3
                       MOVE WS-SCI-GRADE(SCI-IDX,4) TO RP-AVG
                       WRITE SCI-LN FROM RP-DET-LINE
                       SET SCI-IDX UP BY 1
               END-EVALUATE
           END-PERFORM.

       0210-READ-FILE.
           READ INPUT-FILE
               AT END SET EOF TO TRUE
           END-READ.

       0300-FIN.
           CLOSE INPUT-FILE ENG-FILE BUS-FILE IST-FILE SCI-FILE.
           STOP RUN.

       0310-ERR-FIN.
           DISPLAY ERR-MSG.
           DISPLAY ERR-CODE.
           PERFORM 0300-FIN.

       END PROGRAM PracticaDia5.
