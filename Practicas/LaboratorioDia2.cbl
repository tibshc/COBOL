       IDENTIFICATION DIVISION.
       PROGRAM-ID. LaboratorioDia2.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Variables de entrada
           01 WS-STUDENT-COUNT     PIC 9(3).
           01 WS-STUDENT occurs 100 times.
               02 WS-GRADES        pic 9(2)V99 value 0.

      * Variables de resultados
           01 WS-RESULT.
               05 WS-PASS-COUNT        pic 9(3).
               05 WS-FAIL-COUNT        pic 9(3).
               05 WS-EXCELLENT-COUNT   pic 9(3).
               05 WS-GOOD-COUNT        pic 9(3).
               05 WS-REGULAR-COUNT     pic 9(3).
               05 WS-POOR-COUNT        pic 9(3).
               05 WS-TOTAL-GRADE       pic 9(7).
               05 WS-AVERAGE           PIC 9(7)V99.
               05 WS-MAX-GRADE         pic 9(3).
               05 WS-MIN-GRADE         pic 9(3).

      * Indices y control.
           01 WS-INDEX             pic 9(3) value 1.

       PROCEDURE DIVISION.
       
       PRINCIPAL.
           PERFORM INICIO THRU INPUT-GRADES.
           PERFORM ANALYZE-GRADES.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INICIO.
           display "--------------------------------------------".
           display "   lABORATORIO ANALISIS DE CALIFICACIONES".
           display "--------------------------------------------".
           initialize  WS-STUDENT-COUNT WS-RESULT.

       INPUT-STUDENT-COUNT.
           display "INGRESE LA CANTIDAD DE ESTUDIANTES (MAX 100): ".
           perform until WS-STUDENT-COUNT >= 1 and
                         WS-STUDENT-COUNT <= 100
              accept WS-STUDENT-COUNT
              if WS-STUDENT-COUNT < 1 OR WS-STUDENT-COUNT > 100
                  display "EL NUMERO DEBE ESTAR ENTRE 1 A 100"
                  display "INTENTE NUEVAMENTE"
              end-if
           end-perform.


       INPUT-GRADES.
           display "INGRESE LAS CALIFICACIONES DE LOS"
                   WS-STUDENT-COUNT "ESTUDIANTES (0-20): ".
           perform varying WS-INDEX FROM 1 by 1 
                   UNTIL WS-INDEX > WS-STUDENT-COUNT
                   display "CALIFICACION DEL ESTUDIANTE " WS-INDEX ": "
                   perform VALIDATE-GRADE
           end-perform.
           
       VALIDATE-GRADE.
           perform until WS-GRADES(WS-INDEX) > 0 and
                         WS-GRADES(WS-INDEX) <= 20
                 accept WS-GRADES(WS-INDEX)
                 if WS-GRADES(WS-INDEX) < 0 OR 
                 WS-GRADES(WS-INDEX) > 20
                 display "LA CALIFICACION DEBE ESTAR ENTRE 0 Y 20"
                         "INTENTE NUEVAMENTE"

                 end-if
           end-perform.

       ANALYZE-GRADES.
           MOVE 0 TO WS-MAX-GRADE.
           MOVE 20 TO WS-MIN-GRADE.
           perform varying WS-INDEX FROM 1 BY 1 
                   UNTIL WS-INDEX > WS-STUDENT-COUNT
               IF WS-GRADES(WS-INDEX) >= 14
                   add 1 TO WS-PASS-COUNT
                else 
                   ADD 1 TO WS-FAIL-COUNT
               end-if

               if WS-GRADES(WS-INDEX) > WS-MAX-GRADE
                   move WS-GRADES(WS-INDEX) TO WS-MAX-GRADE
               end-if

               if WS-GRADES(WS-INDEX) < WS-MIN-GRADE
                   MOVE WS-GRADES(WS-INDEX) to WS-MIN-GRADE
               end-if


               evaluate true
                when WS-GRADES(WS-INDEX) >= 18
                   add 1 TO WS-EXCELLENT-COUNT
                when WS-GRADES(WS-INDEX) >= 16 and
                     WS-GRADES(WS-INDEX) < 18
                   add 1 TO WS-GOOD-COUNT
                when WS-GRADES(WS-INDEX) >= 14 and
                     WS-GRADES(WS-INDEX) < 16
                   add  1 TO WS-REGULAR-COUNT
                when other
                   add 1 to WS-POOR-COUNT
               end-evaluate

      * aCUMULAR TOTAL
               add WS-GRADES(WS-INDEX) TO WS-TOTAL-GRADE
           end-perform.
           compute WS-AVERAGE = WS-TOTAL-GRADE / WS-STUDENT-COUNT.


       DISPLAY-RESULTS.
       display "------------------------------------------".
       display "RESULTADOS DEL ANALISIS: ".
       display "PROMEDIO GENERAL: " WS-AVERAGE.
       display "CALFICACION MAXIMA: " WS-MAX-GRADE.
       display "CALIFICACION MINIMA: " WS-MIN-GRADE.
       display "TOTAL APROBATORIAS: " WS-PASS-COUNT.
       display "TOTAL REPROBATORIAS: " WS-FAIL-COUNT.
       display "EXCELENTE: " WS-EXCELLENT-COUNT.
       display "BUENO: " WS-GOOD-COUNT.
       display "REGULAR: " WS-REGULAR-COUNT.
       display "MALO: " WS-POOR-COUNT.
       display "------------------------------------------".



       
       END PROGRAM LaboratorioDia2.