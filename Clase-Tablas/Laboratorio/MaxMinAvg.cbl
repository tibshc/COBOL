       IDENTIFICATION DIVISION.
       PROGRAM-ID. MaxMinAvg.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREAS.
           05 WS-CONT                  PIC 9(4).
           05 WS-SUM                   PIC 9(6)V9(3).
           05 WS-MAX                   PIC 9(2)V9(2).
           05 WS-MIN                   PIC 9(2)V9(2).
           05 WS-AVG                   PIC 9(2)V9(2).

       LINKAGE SECTION.
       01  LK-CLASS.
           05  LK-STUDENT OCCURS 10 TIMES INDEXED BY LK-IDX.
               10 LK-ID                PIC 9(7).
               10 LK-LNAME             PIC X(10).
               10 LK-FNAME             PIC X(10).
               10 LK-DEGCOD            PIC X(9).
               10 LK-DEG               PIC X(3).
               10 LK-GRADES.
                   15 LK-GRADE         PIC 9(2)V99 OCCURS 4 TIMES.
       01  LK-MAX                      PIC 9(2)V9(2).
       01  LK-MIN                      PIC 9(2)V9(2).
       01  LK-AVG                      PIC 9(2)V9(2).

       PROCEDURE DIVISION USING LK-CLASS LK-MAX
                                LK-MIN LK-AVG.
           INITIALIZE WS-WORK-AREAS.
           MOVE 20 TO WS-MIN.
           PERFORM VARYING LK-IDX FROM 1 BY 1 UNTIL LK-IDX>10
               IF NOT LK-ID(LK-IDX)=0
                   COMPUTE WS-CONT= 1 + WS-CONT
                   COMPUTE WS-SUM = WS-SUM + LK-GRADE(LK-IDX,4)
                   IF LK-GRADE(LK-IDX,4) < WS-MIN
                       MOVE LK-GRADE(LK-IDX,4) TO WS-MIN
                   END-IF
                   IF LK-GRADE(LK-IDX,4) > WS-MAX
                       MOVE LK-GRADE(LK-IDX,4) TO WS-MAX
                   END-IF
               END-IF

           END-PERFORM.
           COMPUTE WS-AVG = WS-SUM / WS-CONT.
           MOVE WS-MAX TO LK-MAX.
           MOVE WS-MIN TO LK-MIN.
           MOVE WS-AVG TO LK-AVG.

           PERFORM 9000-FINALIZAR.

       9000-FINALIZAR.

       EXIT PROGRAM.
