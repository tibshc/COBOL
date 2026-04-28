       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LabDia5.
       AUTHOR.      EHIDALGO.

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

       FD INPUT-FILE.
       01 STUDENT-INFO.
           88 EOF                 VALUE HIGH-VALUE.
           02 STUDENT-ID          PIC 9(7).
           02 STUDENT-LNAME       PIC X(10).
           02 STUDENT-FNAME       PIC X(10).
           02 STUDENT-DEGCOD      PIC X(9).
           02 STUDENT-DEG         PIC X(3).
           02 STUDENT-GRADE       PIC 9(2)V99 OCCURS 3 TIMES.


       FD  ENG-FILE.
       01  ENG-LN                PIC X(61).

       FD  BUS-FILE.
       01  BUS-LN                PIC X(61).

       FD  IST-FILE.
       01  IST-LN                PIC X(61).

       FD  SCI-FILE.
       01  SCI-LN                PIC X(61).

       
       WORKING-STORAGE SECTION.
     
       01 WS-WORK-AREAS.
           05 FILE-CHECK-KEY   PIC X(2).
           05 ERR-MSG          PIC X(128).
           05 ERR-CODE         PIC X(2).
           05 WS-SUBSCRIPT     PIC 99.
           05 WS-MAX           PIC 9(2)V9(2).
           05 WS-MIN           PIC 9(2)V9(2). 
           05 WS-AVG           PIC 9(2)V9(2). 
           05 WS-OP1           PIC X.
               88 OP1-Y         VALUE "Y".
               88 OP1-ENG       VALUE "1".
               88 OP1-BUS       VALUE "2".
               88 OP1-IST       VALUE "3".
               88 OP1-SCI       VALUE "4". 
           05 WS-SEARCH-NAME   PIC X(10).


       01 WS-ENG-CLASS.   
           05 WS-ENG-STUDENT OCCURS 10 TIMES INDEXED BY ENG-IDX.
              10 WS-ENG-ID         PIC 9(7).
              10 WS-ENG-LNAME      PIC X(10).
              10 WS-ENG-FNAME      PIC X(10).
              10 WS-ENG-DEG-COD    PIC X(9).
              10 WS-ENG-DEG        PIC X(3).
              10 WS-ENG-GRADES.
                 15 WS-ENG-GRADE  PIC 9(2)V99 OCCURS 4 TIMES.


       01 WS-BUS-CLASS.   
           05 WS-BUS-STUDENT OCCURS 10 TIMES INDEXED BY BUS-IDX.
              10 WS-BUS-ID         PIC 9(7).
              10 WS-BUS-LNAME      PIC X(10).
              10 WS-BUS-FNAME      PIC X(10).
              10 WS-BUS-DEG-COD    PIC X(9).
              10 WS-BUS-DEG        PIC X(3).
              10 WS-BUS-GRADES.
                 15 WS-BUS-GRADE  PIC 9(2)V99 OCCURS 4 TIMES.


       01 WS-IST-CLASS.   
           05 WS-IST-STUDENT OCCURS 10 TIMES INDEXED BY IST-IDX.
              10 WS-IST-ID         PIC 9(7).
              10 WS-IST-LNAME      PIC X(10).
              10 WS-IST-FNAME      PIC X(10).
              10 WS-IST-DEG-COD    PIC X(9).
              10 WS-IST-DEG        PIC X(3).
              10 WS-IST-GRADES.
                 15 WS-IST-GRADE  PIC 9(2)V99 OCCURS 4 TIMES.


       01 WS-SCI-CLASS.   
           05 WS-SCI-STUDENT OCCURS 10 TIMES INDEXED BY SCI-IDX.
              10 WS-SCI-ID         PIC 9(7).
              10 WS-SCI-LNAME      PIC X(10).
              10 WS-SCI-FNAME      PIC X(10).
              10 WS-SCI-DEG-COD    PIC X(9).
              10 WS-SCI-DEG        PIC X(3).
              10 WS-SCI-GRADES.
                 15 WS-SCI-GRADE  PIC 9(2)V99 OCCURS 4 TIMES.

       01 RP-FORMAT.
           05 RP-TITLE.
               10 FILLER          PIC X(20) VALUE SPACES.
               10 FILLER          PIC X(14) VALUE "REPORTE CLASE ".
               10 RP-CLASS-NAME   PIC X(3).
               10 FILLER          PIC X(20) VALUE SPACES.

           05 RP-HEADER.
               10 FILLER          PIC X(7) VALUE "ID".
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(10) VALUE "NOMBRE".         
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(10) VALUE "APELLIDO".
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(5) VALUE "N1".
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(5) VALUE "N2".
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(5) VALUE "N3".
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(5) VALUE "AVG".
               10 FILLER          PIC X(2) VALUE SPACES.
           
           05 RP-LINE.
               10 FILLER          PIC X(7) VALUE ALL "-".
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(10) VALUE ALL "-".
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(10) VALUE ALL "-".
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(5) VALUE ALL "-".
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(5) VALUE ALL "-".
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(5) VALUE ALL "-".
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(5) VALUE ALL "-". 

           05 REP-DET.
               10 RP-ID           PIC X(7).
               10 filler          pic X(2) value spaces.
               10 RP-FNAME         pic X(10).
               10 filler           pic X(2) value spaces.
               10 RP-LNAME         pic X(10). 
               10 filler           PIC X(2) value spaces.
               10 RP-N1            pic 9(2)V9(2).
               10 filler           PIC X(2) value spaces.
               10 RP-N2            pic 9(2)V9(2).
               10 filler           PIC X(2) value spaces.
               10 RP-N3            pic 9(2)V9(2).
               10 filler           PIC X(2) value spaces.
               10 RP-AVG           pic 9(2)V9(2).
               10 filler           PIC X(2) value spaces.

           05 RP-MAX.
               10 filler           pic X(9) value spaces.
               10 filler           pic X(13) value "NOTA MAXIMA: ".
               10 RP-N-MAX         pic 9(2)V9(2).
           
         
           05 RP-MIN.
               10 filler           pic X(9) value spaces.
               10 filler           pic X(13) value "NOTA MINIMA: ".
               10 RP-N-MIN         pic 9(2)V9(2).
           
           05 RP-CLASS-AVG.
               10 filler           pic X(9) value spaces.
               10 filler pic X(22) value "PROMEDIO DE LA CLASE: ".
               10 RP-N-AVG         pic 9(2)V9(2).
           
       PROCEDURE DIVISION.
       
       perform 0100-INICIO.
       perform 0200-PROCESO.
       perform 0300-FIN.


       0100-INICIO.
           initialize WS-WORK-AREAS.
           set ENG-IDX TO 1.
           open INPUT INPUT-FILE.
           OPEN output ENG-FILE BUS-FILE IST-FILE SCI-FILE.

           if FILE-CHECK-KEY not = '00'
               move 'Error al abrir INPUT-FILE. CODIGO: ' to ERR-MSG
               move FILE-CHECK-KEY to ERR-CODE
               perform 0310-ERR-FIN
           end-if.
           perform  0110-WRITE-HEADER.
           
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
           MOVE RP-LINE TO ENG-LN BUS-LN IST-LN SCI-LN.
           perform 0220-WRITE-ENG-FILE THRU 0250-WRITE-SCI-FILE. 

       0200-PROCESO.

           PERFORM UNTIL EOF
              PERFORM 0210-READ-FILE
              EVALUATE STUDENT-DEG
                WHEN "ENG"
                   MOVE STUDENT-INFO TO WS-ENG-STUDENT(ENG-IDX) 
                   call "Promedio" 
                       using WS-ENG-GRADES(ENG-IDX)
                   SET ENG-IDX UP BY 1     
                when "BUS"
                   move student-info to ws-bus-student(bus-idx) 
                   call "Promedio" 
                       using ws-bus-grades(bus-idx)
                   SET bus-idx UP BY 1     
                when "IST"
                   move student-info to ws-ist-student(ist-idx) 
                   call "Promedio" 
                       using ws-ist-grades(ist-idx)
                   SET ist-idx UP BY 1     
                when "SCI"
                   move student-info to ws-sci-student(sci-idx) 
                   call "Promedio" 
                       using ws-sci-grades(sci-idx)
                   SET sci-idx UP BY 1     
              END-EVALUATE  
            END-PERFORM. 
            PERFORM 0260-WRITE-ENG-TBL THRU 0263-WRITE-SCI-TBL.
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
               AT END SET EOF TO true
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
              MOVE RP-LINE TO ENG-LN
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
              MOVE RP-LINE TO BUS-LN
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
              MOVE RP-LINE TO IST-LN
              PERFORM 0240-WRITE-IST-FILE
           END-PERFORM. 

       0263-WRITE-SCI-TBL.
           PERFORM VARYING WS-SUBSCRIPT FROM 1 BY 1
                   UNTIL WS-SUBSCRIPT = SCI-IDX
              MOVE WS-SCI-ID(WS-SUBSCRIPT) TO RP-ID
              MOVE WS-SCI-FNAME(WS-SUBSCRIPT) TO RP-FNAME
              MOVE WS-SCI-LNAME(WS-SUBSCRIPT) TO RP-LNAME
              MOVE WS-SCI-GRADE(WS-SUBSCRIPT,1) TO RP-N1
              MOVE WS-SCI-GRADE(WS-SUBSCRIPT,2) TO RP-N2
              MOVE WS-SCI-GRADE(WS-SUBSCRIPT,3) TO RP-N3
              MOVE WS-SCI-GRADE(WS-SUBSCRIPT,4) TO RP-AVG  
              MOVE RP-LINE TO SCI-LN
              PERFORM 0250-WRITE-SCI-FILE
           END-PERFORM. 

       0270-MENU.
           DISPLAY "MENU DE BUSQUEDA".
           DISPLAY "DESEA BUSCAR: SI(Y)".
           ACCEPT WS-OP1.
           IF OP1-Y
               DISPLAY "SELECCIONE LA CLASE EN LA QUE DESEA BUSCAR"
               display "1: ENG"
               display "2: BUS"
               display "3: IST"
               display "4: SCI"
               ACCEPT WS-OP1
               perform 0280-SEARCH
           ELSE
               DISPLAY "NO INGRESO A LA BUSQUEDA"
           END-IF.

       0280-SEARCH. 
           display "INGRESE EL APELLIDO: ".
           ACCEPT WS-SEARCH-NAME.
           EVALUATE true
               when OP1-ENG
                  SET ENG-IDX TO 1
                  SEARCH WS-ENG-STUDENT
                      AT END DISPLAY "NO EXISTE LA PERSONA"
                      WHEN WS-ENG-LNAME(ENG-IDX) = WS-SEARCH-NAME
                           DISPLAY "NOMBRE: " WS-ENG-FNAME(ENG-IDX)
                           DISPLAY "APELLIDO: " WS-ENG-LNAME(ENG-IDX)
                           DISPLAY "ID: " WS-ENG-ID(ENG-IDX)
                           DISPLAY " NOTAS: " WS-ENG-GRADE(ENG-IDX,1)
                           " " WS-ENG-GRADE(ENG-IDX,2) 
                           " " WS-ENG-GRADE(ENG-IDX,3)
                           " " WS-ENG-GRADE(ENG-IDX,4)
                           display "SE ENCUENTRA EN LA FILA"
                                   ENG-IDX "DEL REPORTE"
                  END-SEARCH
               WHEN OP1-BUS
                  SET BUS-IDX TO 1
                  SEARCH WS-BUS-STUDENT
                      AT END DISPLAY "NO EXISTE LA PERSONA"
                      WHEN WS-BUS-LNAME(BUS-IDX) = WS-SEARCH-NAME
                           DISPLAY "NOMBRE: " WS-BUS-FNAME(BUS-IDX)
                           DISPLAY "APELLIDO: " WS-BUS-LNAME(BUS-IDX)
                           DISPLAY "ID: " WS-BUS-ID(BUS-IDX)
                           DISPLAY " NOTAS: " WS-BUS-GRADE(BUS-IDX,1)
                           " " WS-BUS-GRADE(BUS-IDX,2) 
                           " " WS-BUS-GRADE(BUS-IDX,3)
                           " " WS-BUS-GRADE(BUS-IDX,4)
                           display "SE ENCUENTRA EN LA FILA"
                                   BUS-IDX "DEL REPORTE"
                  END-SEARCH                
               WHEN OP1-IST
                  SET IST-IDX TO 1
                  SEARCH WS-IST-STUDENT
                      AT END DISPLAY "NO EXISTE LA PERSONA"
                      WHEN WS-IST-LNAME(IST-IDX) = WS-SEARCH-NAME
                           DISPLAY "NOMBRE: " WS-IST-FNAME(IST-IDX)
                           DISPLAY "APELLIDO: " WS-IST-LNAME(IST-IDX)
                           DISPLAY "ID: " WS-IST-ID(IST-IDX)
                           DISPLAY " NOTAS: " WS-IST-GRADE(IST-IDX,1)
                           " " WS-IST-GRADE(IST-IDX,2) 
                           " " WS-IST-GRADE(IST-IDX,3)
                           " " WS-IST-GRADE(IST-IDX,4)
                           display "SE ENCUENTRA EN LA FILA"
                                   IST-IDX "DEL REPORTE"
                  END-SEARCH   
                WHEN OP1-SCI
                    SET SCI-IDX TO 1
                    SEARCH WS-SCI-STUDENT
                        AT END DISPLAY "NO EXISTE LA PERSONA"
                        WHEN WS-SCI-LNAME(SCI-IDX) = WS-SEARCH-NAME
                            DISPLAY "NOMBRE: " WS-SCI-FNAME(SCI-IDX)
                            DISPLAY "APELLIDO: " WS-SCI-LNAME(SCI-IDX)
                            DISPLAY "ID: " WS-SCI-ID(SCI-IDX)
                            DISPLAY " NOTAS: " WS-SCI-GRADE(SCI-IDX,1)
                            " " WS-SCI-GRADE(SCI-IDX,2) 
                            " " WS-SCI-GRADE(SCI-IDX,3)
                            " " WS-SCI-GRADE(SCI-IDX,4)
                            display "SE ENCUENTRA EN LA FILA"
                                    SCI-IDX "DEL REPORTE"
                    END-SEARCH   
                WHEN OTHER
                    DISPLAY "ERROR EN LA OPCION" WS-OP1
           END-EVALUATE.

       0300-FIN.
           CLOSE INPUT-FILE ENG-FILE BUS-FILE IST-FILE SCI-FILE.
           perform 0270-MENU.
           STOP RUN. 
       0310-ERR-FIN.
           display ERR-MSG.
           display ERR-CODE.
           perform  0300-FIN.
           
       END PROGRAM LabDia5.
