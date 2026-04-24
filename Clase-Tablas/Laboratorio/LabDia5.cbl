       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LabDia5.
       AUTHOR.      EHIDALGO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "STUDENTS_5.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
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
               10 FILLER          PIC X(10)VALUE "NOMBRE".         
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(10)VALUE "APELLIDO".
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
               10 FILLER          PIC X(10)VALUE ALL "-".
               10 FILLER          PIC X(2) VALUE SPACES.
               10 FILLER          PIC X(10)VALUE ALL "-".
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
               10 filler           pic X(13) value "NOTA MAXIMA: "
               10 RP-N-MAX         pic 9(2)V9(2).
           
         
           05 RP-MIN.
               10 filler           pic X(9) value spaces.
               10 filler           pic X(13) value "NOTA MINIMA: "
               10 RP-N-MIN         pic 9(2)V9(2).
           
           05 RP-CLASS-AVG.
               10 filler           pic X(9) value spaces.
               10 filler           pic X(13) value "PROMEDIO "
                                   "DE LA CLASE: ".
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
           

       END PROGRAM LabDia5.