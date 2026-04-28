       IDENTIFICATION DIVISION.
       PROGRAM-ID. PracticaDia5.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "STUDENTS_5.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-CHECK-KEY.

           SELECT ENG-FILE ASSIGN TO "STUDENTS_ENG.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT BUS-FILE ASSIGN TO "STUDENTS_BUS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT IST-FILE ASSIGN TO "STUDENTS_IST.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SCI-FILE ASSIGN TO "STUDENTS_SCI.DAT"
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
       01  ENG-CLASS.
           02  ENG-ID           PIC 9(7).
           02  ENG-LNAME        PIC X(10).
           02  ENG-FNAME        PIC X(10).
           02  ENG-DEGCOD       PIC X(9).
           02  ENG-DEG          PIC X(3).
           02  ENG-GRADES       OCCURS 4 TIMES.
               03  ENG-GRADE    PIC 9(2)V99.

       FD  BUS-FILE.
       01  BUS-CLASS.
           02  BUS-ID           PIC 9(7).
           02  BUS-LNAME        PIC X(10).
           02  BUS-FNAME        PIC X(10).
           02  BUS-DEGCOD       PIC X(9).
           02  BUS-DEG          PIC X(3).
           02  BUS-GRADES       OCCURS 4 TIMES.
               03  BUS-GRADE    PIC 9(2)V99.

       FD  IST-FILE.
       01  IST-CLASS.
           02  IST-ID           PIC 9(7).
           02  IST-LNAME        PIC X(10).
           02  IST-FNAME        PIC X(10).
           02  IST-DEGCOD       PIC X(9).
           02  IST-DEG          PIC X(3).
           02  IST-GRADES       OCCURS 4 TIMES.
               03  IST-GRADE    PIC 9(2)V99.

       FD  SCI-FILE.
       01  SCI-CLASS.
           02  SCI-ID           PIC 9(7).
           02  SCI-LNAME        PIC X(10).
           02  SCI-FNAME        PIC X(10).
           02  SCI-DEGCOD       PIC X(9).
           02  SCI-DEG          PIC X(3).
           02  SCI-GRADES       OCCURS 4 TIMES.
               03  SCI-GRADE    PIC 9(2)V99.

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREAS.
           05  FILE-CHECK-KEY   PIC X(2).
           05  ERR-MSG          PIC X(128).
           05  ERR-CODE         PIC X(2).
           05  WS-SUBSCRIPT     PIC 99.
           05  WS-OP1           PIC X.
               88  OP1-Y        VALUE "y".
               88  OP1-ENG      VALUE "1".
               88  OP1-BUS      VALUE "2".
               88  OP1-IST      VALUE "3".
               88  OP1-SCI      VALUE "4".
           05  WS-SEARCH-NAME   PIC X(10).

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
                       CALL "Promedio"
                           USING WS-ENG-GRADES(ENG-IDX)
                       WRITE ENG-CLASS FROM WS-ENG-STUDENT(ENG-IDX)
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
                       CALL "Promedio"
                           USING WS-BUS-GRADES(BUS-IDX)
                       WRITE BUS-CLASS FROM WS-BUS-STUDENT(BUS-IDX)
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
                       CALL "Promedio"
                           USING WS-IST-GRADES(IST-IDX)
                       WRITE IST-CLASS FROM WS-IST-STUDENT(IST-IDX)
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
                       CALL "Promedio"
                           USING WS-SCI-GRADES(SCI-IDX)
                       WRITE SCI-CLASS FROM WS-SCI-STUDENT(SCI-IDX)
                       SET SCI-IDX UP BY 1
               END-EVALUATE
           END-PERFORM.
           PERFORM 0220-MENU.

       0210-READ-FILE.
           READ INPUT-FILE
               AT END SET EOF TO TRUE
           END-READ.

       0220-MENU.
           DISPLAY "MENU DE BUSQUEDA".
           DISPLAY "Desea buscar: Si(y)".
           ACCEPT WS-OP1.
           IF OP1-Y
               DISPLAY "Seleccione la clase en la que desea buscar:"
               DISPLAY "1: ENG"
               DISPLAY "2: BUS"
               DISPLAY "3: IST"
               DISPLAY "4: SCI"
               ACCEPT WS-OP1
               PERFORM 0230-SEARCH
           ELSE
               DISPLAY "No ingreso a la busqueda"
           END-IF.

       0230-SEARCH.
           DISPLAY "Ingrese el nombre:".
           ACCEPT WS-SEARCH-NAME.
           EVALUATE TRUE
               WHEN OP1-ENG
                   SET ENG-IDX TO 1
                   SEARCH WS-ENG-STUDENT
                       AT END DISPLAY "No existe la persona"
                                      " en la clase ENG."
                       WHEN WS-ENG-FNAME(ENG-IDX) = WS-SEARCH-NAME
                           DISPLAY "Existe la persona "
                                   WS-SEARCH-NAME " con id: "
                                   WS-ENG-ID(ENG-IDX)
                           DISPLAY "Se encuentra en la fila "
                                   ENG-IDX " del reporte"
                   END-SEARCH
               WHEN OP1-BUS
                   SET BUS-IDX TO 1
                   SEARCH WS-BUS-STUDENT
                       AT END DISPLAY "No existe la persona"
                                      " en la clase BUS."
                       WHEN WS-BUS-FNAME(BUS-IDX) = WS-SEARCH-NAME
                           DISPLAY "Existe la persona "
                                   WS-SEARCH-NAME " con id: "
                                   WS-BUS-ID(BUS-IDX)
                           DISPLAY "Se encuentra en la fila "
                                   BUS-IDX " del reporte"
                   END-SEARCH
               WHEN OP1-IST
                   SET IST-IDX TO 1
                   SEARCH WS-IST-STUDENT
                       AT END DISPLAY "No existe la persona"
                                      " en la clase IST."
                       WHEN WS-IST-FNAME(IST-IDX) = WS-SEARCH-NAME
                           DISPLAY "Existe la persona "
                                   WS-SEARCH-NAME " con id: "
                                   WS-IST-ID(IST-IDX)
                           DISPLAY "Se encuentra en la fila "
                                   IST-IDX " del reporte"
                   END-SEARCH
               WHEN OP1-SCI
                   SET SCI-IDX TO 1
                   SEARCH WS-SCI-STUDENT
                       AT END DISPLAY "No existe la persona"
                                      " en la clase SCI."
                       WHEN WS-SCI-FNAME(SCI-IDX) = WS-SEARCH-NAME
                           DISPLAY "Existe la persona "
                                   WS-SEARCH-NAME " con id: "
                                   WS-SCI-ID(SCI-IDX)
                           DISPLAY "Se encuentra en la fila "
                                   SCI-IDX " del reporte"
                   END-SEARCH
           END-EVALUATE.

       0300-FIN.
           CLOSE INPUT-FILE ENG-FILE BUS-FILE IST-FILE SCI-FILE.
           STOP RUN.

       0310-ERR-FIN.
           DISPLAY ERR-MSG.
           DISPLAY ERR-CODE.
           PERFORM 0300-FIN.

       END PROGRAM PracticaDia5.
