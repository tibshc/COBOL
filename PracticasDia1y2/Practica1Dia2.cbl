       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRACTICA1DIA2. 
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Variables del programa
           01 WS-STUDENT-COUNT         pic 9(2) value 3.
           01 WS-STUDENT occurs 99 times.
               02 WS-GRADES            pic 9(3) VALUE 0.
                   88 WS-APROBADO      values are 60 thru 100.
                   88 WS-REPROBADO     values are 0 THRU 100.

           01 WS-RESULT.
               03 WS-PASS-COUNT        PIC 9(2).
               03 WS-FAIL-COUNT        PIC 9(2).
               03 WS-ALTO-COUNT        PIC 9(2).
               03 WS-BAJO-COUNT        pic 9(2).
               03 WS-EXCELLENT-COUNT   pic 9(3).
               03 WS-GOOD-COUNT        pic 9(3).
               03 WS-REGULAR-COUNT     pic 9(3).
               03 WS-POOR-COUNT        pic 9(3).


           01 WS-INDEX                 PIC 9(2) value 1.
       
       PROCEDURE DIVISION.
       
       PRINCIPAL.
           DISPLAY "Iniciando programa...".
           perform INICIO THRU INPUT-GRADES.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.
       
       INICIO.
           display "-----------------------------------------".
           display "   PRACTICA 1 DIA 2"
           display "-----------------------------------------".
           initialize WS-RESULT.

       INPUT-GRADES.
           DISPLAY "INGRESE " WS-STUDENT-COUNT 
           "CALIFICACIONES DEL 1 AL 100: "
           PERFORM VARYING WS-INDEX FROM 1 BY 1 
                   until WS-INDEX > WS-STUDENT-COUNT
                   display "INGRESE CALIFICACION " WS-INDEX ": "
                   perform VALIDATE-GRADE
                   perform ANALIZE-GRADE
                   perform EVALUATE-GRADE
                   display ""
           end-perform.


       VALIDATE-GRADE.
           perform until WS-GRADES(WS-INDEX) > 0 and
                         WS-GRADES(WS-INDEX) <= 100
                 accept WS-GRADES(WS-INDEX)
                 if WS-GRADES(WS-INDEX) < 0 OR 
                 WS-GRADES(WS-INDEX) > 100
                 display "LA CALIFICACION DEBE ESTAR ENTRE 0 Y 100 "
                         "INTENTE NUEVAMENTE"

              end-if
           end-perform.

           
       ANALIZE-GRADE.
           if WS-GRADES(WS-INDEX) >= 80
               display "LA CALIFICACION ES ALTA!"
               add 1 to WS-ALTO-COUNT
           else
               display "LA CALIFICACION ES BAJA"
               add 1 TO WS-BAJO-COUNT
           end-if.
           
           if WS-APROBADO(WS-INDEX)
               add 1 TO WS-PASS-COUNT
               display "ESTADO: APROBADO"
           else
               add 1 TO WS-FAIL-COUNT
               display "ESTADO: REPROBADO"
           end-if.

       EVALUATE-GRADE.

           evaluate true
                when WS-GRADES(WS-INDEX) >= 90
                   add 1 TO WS-EXCELLENT-COUNT
                when WS-GRADES(WS-INDEX) >= 75 and
                     WS-GRADES(WS-INDEX) < 90
                   add 1 TO WS-GOOD-COUNT
                when WS-GRADES(WS-INDEX) >= 60 and
                     WS-GRADES(WS-INDEX) < 75
                   add  1 TO WS-REGULAR-COUNT
                when other
                   add 1 to WS-POOR-COUNT
               end-evaluate.

       DISPLAY-RESULTS.
           display "--------------------------------------".
           display "           RESULTADOS".
           display "--------------------------------------".
           display "**************************************".
           perform  varying WS-INDEX from 1 by 1 
                    until WS-INDEX > WS-STUDENT-COUNT
                    display "NOTA ("WS-INDEX "): " WS-GRADES(WS-INDEX)
           END-PERFORM.         
           display "**************************************"
           display "--------------------------------------"
           display "CALIFICACIONES APROBATORIAS: " WS-PASS-COUNT.
           display "CALIFICACIONES REPROBATORIAS: " WS-FAIL-COUNT.
           display "--------------------------------------"
           display "CALIFICACIONES EXCELENTES: " WS-EXCELLENT-COUNT.
           display "CALIFICACIONES BUENAS: " WS-GOOD-COUNT.
           display "CALIFICACIONES REGULARES: " WS-REGULAR-COUNT.
           display "CALIFICACIONES MALAS: "  WS-POOR-COUNT.
           display "--------------------------------------".
         
               


       END PROGRAM PRACTICA1DIA2. 