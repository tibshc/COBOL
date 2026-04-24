       IDENTIFICATION DIVISION.
       PROGRAM-ID. LaboratorioDia5.

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
           88  EOF                     VALUE HIGH-VALUE.
           02  STUDENT-ID              PIC 9(7).
           02  STUDENT-LNAME           PIC X(10).
           02  STUDENT-FNAME           PIC X(10).
           02  STUDENT-DEGCOD          PIC X(9).
           02  STUDENT-DEG             PIC X(3).
           02  STUDENT-GRADE           PIC 9(2)V99 OCCURS 3 TIMES.

       FD  ENG-FILE.
       01  ENG-LN                      PIC X(61).

       FD  BUS-FILE.
       01  BUS-LN                      PIC X(61).

       FD  IST-FILE.
       01  IST-LN                      PIC X(61).

       FD  SCI-FILE.
       01  SCI-LN                      PIC X(61).

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREAS.
           05  FILE-CHECK-KEY          PIC X(2).
           05  ERR-MSG                 PIC X(128).
           05  ERR-CODE                PIC X(2).
           05  WS-SUBSCRIPT            PIC 99.
           05  WS-MAX                  PIC 9(2)V9(2).
           05  WS-MIN                  PIC 9(2)V9(2).
           05  WS-AVG                  PIC 9(2)V9(2).
           05  WS-OP1                  PIC X.
               88  OP1-Y               VALUE "y".
               88  OP1-ENG             VALUE "1".
               88  OP1-BUS             VALUE "2".
               88  OP1-IST             VALUE "3".
               88  OP1-SCI             VALUE "4".
           05  WS-SEARCH-NAME          PIC X(10).

       01  WS-ENG-CLASS.
           05  WS-ENG-STUDENT OCCURS 10 TIMES INDEXED BY ENG-IDX.
               10 WS-ENG-ID            PIC 9(7).
               10 WS-ENG-LNAME         PIC X(10).
               10 WS-ENG-FNAME         PIC X(10).
               10 WS-ENG-DEGCOD        PIC X(9).
               10 WS-ENG-DEG           PIC X(3).
               10 WS-ENG-GRADES.
                   15 WS-ENG-GRADE     PIC 9(2)V99 OCCURS 4 TIMES.

       01  WS-BUS-CLASS.
           05  WS-BUS-STUDENT OCCURS 10 TIMES INDEXED BY BUS-IDX.
               10 WS-BUS-ID            PIC 9(7).
               10 WS-BUS-LNAME         PIC X(10).
               10 WS-BUS-FNAME         PIC X(10).
               10 WS-BUS-DEGCOD        PIC X(9).
               10 WS-BUS-DEG           PIC X(3).
               10 WS-BUS-GRADES.
                   15 WS-BUS-GRADE     PIC 9(2)V99 OCCURS 4 TIMES.

       01  WS-IST-CLASS.
           05  WS-IST-STUDENT OCCURS 10 TIMES INDEXED BY IST-IDX.
               10 WS-IST-ID            PIC 9(7).
               10 WS-IST-LNAME         PIC X(10).
               10 WS-IST-FNAME         PIC X(10).
               10 WS-IST-DEGCOD        PIC X(9).
               10 WS-IST-DEG           PIC X(3).
               10 WS-IST-GRADES.
                   15 WS-IST-GRADE     PIC 9(2)V99 OCCURS 4 TIMES.

       01  WS-SCI-CLASS.
           05  WS-SCI-STUDENT OCCURS 10 TIMES INDEXED BY SCI-IDX.
               10 WS-SCI-ID            PIC 9(7).
               10 WS-SCI-LNAME         PIC X(10).
               10 WS-SCI-FNAME         PIC X(10).
               10 WS-SCI-DEGCOD        PIC X(9).
               10 WS-SCI-DEG           PIC X(3).
               10 WS-SCI-GRADES.
                   15 WS-SCI-GRADE     PIC 9(2)V99 OCCURS 4 TIMES.

       01  RP-FORMAT.
           05  RP-TITLE.
               10  FILLER              PIC X(20) VALUE SPACES.
               10  FILLER              PIC X(14) VALUE "REPORTE CLASE ".
               10  RP-CLASS-NAME       PIC X(3).
               10  FILLER              PIC X(20) VALUE SPACES.
           05  RP-HEADER.
               10  FILLER              PIC X(7) VALUE "ID".
               10  FILLER              PIC X(2) VALUE SPACES.
               10  FILLER              PIC X(10) VALUE "NOMBRE".
               10  FILLER              PIC X(2) VALUE SPACES.
               10  FILLER              PIC X(10) VALUE "APELLIDO".
               10  FILLER              PIC X(2) VALUE SPACES.
               10  FILLER              PIC X(5) VALUE "N1".
               10  FILLER              PIC X(2) VALUE SPACES.
               10  FILLER              PIC X(5) VALUE "N2".
               10  FILLER              PIC X(2) VALUE SPACES.
               10  FILLER              PIC X(5) VALUE "N3".
               10  FILLER              PIC X(2) VALUE SPACES.
               10  FILLER              PIC X(5) VALUE "AVG".
               10  FILLER              PIC X(2) VALUE SPACES.
           05  RP-HEADER-LN.
               10  FILLER              PIC X(7) VALUE ALL "-".
               10  FILLER              PIC X(2) VALUE SPACES.
               10  FILLER              PIC X(10) VALUE ALL "-".
               10  FILLER              PIC X(2) VALUE SPACES.
               10  FILLER              PIC X(10) VALUE ALL "-".
               10  FILLER              PIC X(2) VALUE SPACES.
               10  FILLER              PIC X(5) VALUE ALL "-".
               10  FILLER              PIC X(2) VALUE SPACES.
               10  FILLER              PIC X(5) VALUE ALL "-".
               10  FILLER              PIC X(2) VALUE SPACES.
               10  FILLER              PIC X(5) VALUE ALL "-".
               10  FILLER              PIC X(2) VALUE SPACES.
               10  FILLER              PIC X(5) VALUE ALL "-".
               10  FILLER              PIC X(2) VALUE SPACES.
           05  RP-DET.
               10  RP-ID               PIC X(7).
               10  FILLER              PIC X(2) VALUE SPACES.
               10  RP-FNAME            PIC X(10).
               10  FILLER              PIC X(2) VALUE SPACES.
               10  RP-LNAME            PIC X(10).
               10  FILLER              PIC X(2) VALUE SPACES.
               10  RP-N1               PIC 9(2).9(2).
               10  FILLER              PIC X(2) VALUE SPACES.
               10  RP-N2               PIC 9(2).9(2).
               10  FILLER              PIC X(2) VALUE SPACES.
               10  RP-N3               PIC 9(2).9(2).
               10  FILLER              PIC X(2) VALUE SPACES.
               10  RP-AVG              PIC 9(2).9(2).
               10  FILLER              PIC X(2) VALUE SPACES.
           05  RP-MAX.
               10  FILLER          PIC X(9) VALUE SPACES.
               10  FILLER              PIC X(13) VALUE "NOTA MAXIMA: ".
               10  RP-N-MAX            PIC 9(2).9(2).
           05  RP-MIN.
                   10  FILLER          PIC X(9) VALUE SPACES.
                   10  FILLER          PIC X(13) VALUE "NOTA MINIMA: ".
                   10  RP-N-MIN        PIC 9(2).9(2).
           05  RP-CLASS-AVG.
                   10  FILLER          PIC X(22) VALUE "PROMEDIO "-
                                       "DE LA CLASE: ".
                   10  RP-N-AVG        PIC 9(2).9(2).
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

           PERFORM 0110-WRITE-HEADER.

       0110-WRITE-HEADER.
           MOVE "ENG" TO RP-CLASS-NAME.
           MOVE RP-TITLE TO ENG-LN.
           MOVE "BUS" TO RP-CLASS-NAME.
           MOVE RP-TITLE TO BUS-LN.
           MOVE "IST" TO RP-CLASS-NAME.
           MOVE RP-TITLE TO IST-LN.
           MOVE "SCI" TO RP-CLASS-NAME.
           MOVE RP-TITLE TO SCI-LN.
           PERFORM 0220-WRITE-ENG-FILE THRU 0250-WRITE-SCI-FILE.
           MOVE RP-HEADER TO ENG-LN BUS-LN IST-LN SCI-LN.
           PERFORM 0220-WRITE-ENG-FILE THRU 0250-WRITE-SCI-FILE.
           MOVE RP-HEADER-LN TO ENG-LN BUS-LN IST-LN SCI-LN.
           PERFORM 0220-WRITE-ENG-FILE THRU 0250-WRITE-SCI-FILE.
       0200-PROCESO.

           PERFORM UNTIL EOF
               PERFORM 0210-READ-FILE
               EVALUATE STUDENT-DEG
                   WHEN "ENG"
                       MOVE STUDENT-INFO TO WS-ENG-STUDENT(ENG-IDX)
                       CALL "Promedio"
                           USING WS-ENG-GRADES(ENG-IDX)
                       SET ENG-IDX UP BY 1
                   WHEN "BUS"
                       MOVE STUDENT-INFO TO WS-BUS-STUDENT(BUS-IDX)
                       CALL "Promedio"
                           USING WS-BUS-GRADES(BUS-IDX)
                       SET BUS-IDX UP BY 1
                   WHEN "IST"
                       MOVE STUDENT-INFO TO WS-IST-STUDENT(IST-IDX)
                       CALL "Promedio"
                           USING WS-IST-GRADES(IST-IDX)
                       SET IST-IDX UP BY 1
                   WHEN "SCI"
                       MOVE STUDENT-INFO TO WS-SCI-STUDENT(SCI-IDX)
                       CALL "Promedio"
                           USING WS-SCI-GRADES(SCI-IDX)
                       SET SCI-IDX UP BY 1
               END-EVALUATE
           END-PERFORM.
           PERFORM 0260-WRITE-ENG-TBL THRU 0263-WRITE-IST-TBL.
           MOVE ALL "-" TO ENG-LN BUS-LN IST-LN SCI-LN.
           PERFORM 0220-WRITE-ENG-FILE THRU 0250-WRITE-SCI-FILE.

           CALL "MaxMinAvg"
               USING WS-ENG-CLASS WS-MAX WS-MIN WS-AVG.
           MOVE WS-MAX TO RP-N-MAX.
           MOVE WS-MIN TO RP-N-MIN.
           MOVE WS-AVG TO RP-N-AVG.
           WRITE ENG-LN FROM RP-MAX.
           WRITE ENG-LN FROM RP-MIN.
           WRITE ENG-LN FROM RP-CLASS-AVG.

           CALL "MaxMinAvg"
               USING WS-BUS-CLASS WS-MAX WS-MIN WS-AVG.
           MOVE WS-MAX TO RP-N-MAX.
           MOVE WS-MIN TO RP-N-MIN.
           MOVE WS-AVG TO RP-N-AVG.
           WRITE BUS-LN FROM RP-MAX.
           WRITE BUS-LN FROM RP-MIN.
           WRITE BUS-LN FROM RP-CLASS-AVG.

           CALL "MaxMinAvg"
               USING WS-IST-CLASS WS-MAX WS-MIN WS-AVG.
           MOVE WS-MAX TO RP-N-MAX.
           MOVE WS-MIN TO RP-N-MIN.
           MOVE WS-AVG TO RP-N-AVG.
           WRITE IST-LN FROM RP-MAX.
           WRITE IST-LN FROM RP-MIN.
           WRITE IST-LN FROM RP-CLASS-AVG.

           CALL "MaxMinAvg"
               USING WS-SCI-CLASS WS-MAX WS-MIN WS-AVG.
           MOVE WS-MAX TO RP-N-MAX.
           MOVE WS-MIN TO RP-N-MIN.
           MOVE WS-AVG TO RP-N-AVG.
           WRITE SCI-LN FROM RP-MAX.
           WRITE SCI-LN FROM RP-MIN.
           WRITE SCI-LN FROM RP-CLASS-AVG.


       0210-READ-FILE.
           READ INPUT-FILE
               AT END SET EOF TO TRUE
           END-READ.

       0220-WRITE-ENG-FILE.
           WRITE ENG-LN.
       0230-WRITE-BUS-FILE.
           WRITE BUS-LN.
       0240-WRITE-IST-FILE.
           WRITE IST-LN.
       0250-WRITE-SCI-FILE.
           WRITE SCI-LN.

       0260-WRITE-ENG-TBL.
           PERFORM VARYING WS-SUBSCRIPT FROM 1 BY 1
                   UNTIL WS-SUBSCRIPT = ENG-IDX
               MOVE WS-ENG-ID(WS-SUBSCRIPT) TO RP-ID
               MOVE WS-ENG-FNAME(WS-SUBSCRIPT) TO RP-FNAME
               MOVE WS-ENG-LNAME(WS-SUBSCRIPT) TO RP-LNAME
               MOVE WS-ENG-GRADE(WS-SUBSCRIPT,1) TO RP-N1
               MOVE WS-ENG-GRADE(WS-SUBSCRIPT,2) TO RP-N2
               MOVE WS-ENG-GRADE(WS-SUBSCRIPT,3) TO RP-N3
               MOVE WS-ENG-GRADE(WS-SUBSCRIPT,4) TO RP-AVG
               MOVE RP-DET TO ENG-LN
               PERFORM 0220-WRITE-ENG-FILE
           END-PERFORM.

       0261-WRITE-BUS-TBL.
           PERFORM VARYING WS-SUBSCRIPT FROM 1 BY 1
                   UNTIL WS-SUBSCRIPT = BUS-IDX
               MOVE WS-BUS-ID(WS-SUBSCRIPT) TO RP-ID
               MOVE WS-BUS-FNAME(WS-SUBSCRIPT) TO RP-FNAME
               MOVE WS-BUS-LNAME(WS-SUBSCRIPT) TO RP-LNAME
               MOVE WS-BUS-GRADE(WS-SUBSCRIPT,1) TO RP-N1
               MOVE WS-BUS-GRADE(WS-SUBSCRIPT,2) TO RP-N2
               MOVE WS-BUS-GRADE(WS-SUBSCRIPT,3) TO RP-N3
               MOVE WS-BUS-GRADE(WS-SUBSCRIPT,4) TO RP-AVG
               MOVE RP-DET TO BUS-LN
               PERFORM 0230-WRITE-BUS-FILE
           END-PERFORM.

       0262-WRITE-IST-TBL.
           PERFORM VARYING WS-SUBSCRIPT FROM 1 BY 1
                   UNTIL WS-SUBSCRIPT = IST-IDX
               MOVE WS-IST-ID(WS-SUBSCRIPT) TO RP-ID
               MOVE WS-IST-FNAME(WS-SUBSCRIPT) TO RP-FNAME
               MOVE WS-IST-LNAME(WS-SUBSCRIPT) TO RP-LNAME
               MOVE WS-IST-GRADE(WS-SUBSCRIPT,1) TO RP-N1
               MOVE WS-IST-GRADE(WS-SUBSCRIPT,2) TO RP-N2
               MOVE WS-IST-GRADE(WS-SUBSCRIPT,3) TO RP-N3
               MOVE WS-IST-GRADE(WS-SUBSCRIPT,4) TO RP-AVG
               MOVE RP-DET TO IST-LN
               PERFORM 0240-WRITE-IST-FILE
           END-PERFORM.

       0263-WRITE-IST-TBL.
           PERFORM VARYING WS-SUBSCRIPT FROM 1 BY 1
                   UNTIL WS-SUBSCRIPT = SCI-IDX
               MOVE WS-SCI-ID(WS-SUBSCRIPT) TO RP-ID
               MOVE WS-SCI-FNAME(WS-SUBSCRIPT) TO RP-FNAME
               MOVE WS-SCI-LNAME(WS-SUBSCRIPT) TO RP-LNAME
               MOVE WS-SCI-GRADE(WS-SUBSCRIPT,1) TO RP-N1
               MOVE WS-SCI-GRADE(WS-SUBSCRIPT,2) TO RP-N2
               MOVE WS-SCI-GRADE(WS-SUBSCRIPT,3) TO RP-N3
               MOVE WS-SCI-GRADE(WS-SUBSCRIPT,4) TO RP-AVG
               MOVE RP-DET TO SCI-LN
               PERFORM 0250-WRITE-SCI-FILE
           END-PERFORM.

       0270-MENU.
           DISPLAY "MENU DE BUSQUEDA".
           DISPLAY "Desea buscar: Si(y)".
           ACCEPT WS-OP1.
           IF OP1-Y
               DISPLAY "Seleccione la clase en la que desea buscar:"
               DISPLAY "1: ENG"
               DISPLAY "2: BUS"
               DISPLAY "3: IST"
               DISPLAY "4: SCI"
               ACCEPT  WS-OP1
               PERFORM 0280-SEARCH
           ELSE
               DISPLAY "No ingreso a la busqueda".

       0280-SEARCH.
           DISPLAY "Ingrese el apellido:".
           ACCEPT WS-SEARCH-NAME.
           EVALUATE TRUE
               WHEN OP1-ENG
                   SET ENG-IDX TO 1
                   SEARCH WS-ENG-STUDENT
                          AT END DISPLAY "No existe la persona"
                                         " en la clase ENG."
                       WHEN WS-ENG-LNAME(ENG-IDX)=WS-SEARCH-NAME
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
                       WHEN WS-BUS-LNAME(BUS-IDX)=WS-SEARCH-NAME
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
                       WHEN WS-IST-LNAME(IST-IDX)=WS-SEARCH-NAME
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
                       WHEN WS-SCI-LNAME(SCI-IDX)=WS-SEARCH-NAME
                           DISPLAY "Existe la persona "
                                   WS-SEARCH-NAME " con id: "
                                   WS-SCI-ID(SCI-IDX)
                           DISPLAY "Se encuentra en la fila "
                                   SCI-IDX " del reporte"
                   END-SEARCH
           END-EVALUATE.

       0300-FIN.
           CLOSE INPUT-FILE ENG-FILE BUS-FILE IST-FILE SCI-FILE.
           PERFORM 0270-MENU.
           STOP RUN.
       0310-ERR-FIN.
           DISPLAY ERR-MSG.
           DISPLAY ERR-CODE.
           PERFORM 0300-FIN.

       END PROGRAM LaboratorioDia5.
