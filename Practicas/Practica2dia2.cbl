       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRACTICA2DIA2.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Variables del programa     
           01 WS-NUMBER-COUNT      PIC 9(3) VALUE 5.    
           01 WS-NUMBER            pic 9(3)  occurs 99 times.
           01 WS-NUMBER-LIMIT      pic s9(3).

           01 WS-RESULT.
               05 WS-SUMA          pic 9(5).
               05 WS-PROMEDIO      pic 9(3).
               05 WS-BIG-COUNT     pic 9(3).

           01 WS-INDEX             pic 9(3) value 1.

       
       PROCEDURE DIVISION.
       
       PRINCIPAL.
           perform INICIO THRU DISPLAY-RESULTS.
           
           STOP RUN.
       
       INICIO. 
           display "--------------------------------------------".
           display "   PRACTICA 2 DIA 2".
           display "--------------------------------------------".
           initialize WS-RESULT WS-NUMBER-LIMIT.


       INPUT-NUMBERS.
           display "INGRESE LOS " WS-NUMBER-COUNT " NUMEROS"
           PERFORM varying WS-INDEX FROM 1 BY 1 
                  until WS-INDEX > WS-NUMBER-COUNT
                  display "NUMERO " WS-INDEX ": "
                  accept WS-NUMBER(WS-INDEX)
           end-perform.
       
       VALIDATE-LIMIT.
           perform until WS-NUMBER-LIMIT > 0
               display "INGRESE UN VALOR LIMITE POSITIVO"
               accept WS-NUMBER-LIMIT
               if WS-NUMBER-LIMIT <= 0 
                   display "EL NUMERO DEBE SER POSITIVO"
               end-if
           end-perform.

       CALCULAR-SUMA.
           PERFORM varying WS-INDEX FROM 1 BY 1 
               until WS-INDEX > WS-NUMBER-COUNT
               add WS-NUMBER(WS-INDEX) to WS-SUMA
           end-perform.
           compute WS-PROMEDIO = WS-SUMA/WS-NUMBER-COUNT.
       
       CONTAR-LIMITE.
           perform varying WS-INDEX from 1 BY 1 
               until WS-INDEX > WS-NUMBER-COUNT
               if WS-NUMBER(WS-INDEX) > WS-NUMBER-LIMIT
                  add 1 TO WS-BIG-COUNT
               end-if
           end-perform.

       DISPLAY-RESULTS.
           display "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
           display "------------------------------------------".
           display "               RESULTADOS: "
           display "------------------------------------------"
           display "******************************************"
           perform varying WS-INDEX from 1 by 1 
                   until WS-INDEX > WS-NUMBER-COUNT
                   display "NUMERO(" WS-INDEX "): " WS-NUMBER(WS-INDEX)
           end-perform.
           display "******************************************"
           display "NUMERO LIMITE: " WS-NUMBER-LIMIT.
           display "******************************************"
           display "------------------------------------------"
           display "RESULTADO SUMA: " WS-SUMA.
           display "RESULTADO PROMEDIO: " WS-PROMEDIO.
           display "NUMEROS MAYORES QUE LIMITE: " WS-BIG-COUNT.
           display "------------------------------------------".
           display "//////////////////////////////////////////".
       END PROGRAM PRACTICA2DIA2.