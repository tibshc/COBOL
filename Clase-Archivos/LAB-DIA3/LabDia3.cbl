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
               
       CONFIGURATION SECTION.
       
       DATA DIVISION.
       file section.
       
       fd  INPUT-FILE
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
                




       
       PROCEDURE DIVISION.
       
       PRINCIPAL.
           DISPLAY "Iniciando programa...".
           
           STOP RUN.
       
       END PROGRAM LaboratorioDia3.