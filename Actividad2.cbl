       identification division.
       program-id. PRACTICA2.
       
       environment division.
       configuration section.
       
       data division.
       working-storage section.
           01 WS-NOMBRE        pic X(30) value zero.
           01 WS-NUMERO1       pic 9(2) value zero.
           01 WS-NUMERO2       pic 9(2) value ZERO.
           01 WS-NUMERO3       PIC 9(2) VALUE zero.
           01 WS-RESULTADO.  
               02 WS-RESULTADO-SUMA        PIC 9(4) VALUE ZERO.
               02 WS-RESULTADO-MULTI       PIC 9(5) VALUE ZERO.
               02 WS-RESULTADO-MEDIA       PIC 9(4) VALUE zero.
               02 WS-RESULTADO-DOBLE.       
                   04 WS-DOBLE1            PIC 9(4) VALUE ZERO.
                   04 WS-DOBLE2            PIC 9(4) VALUE ZERO.
                   04 WS-DOBLE3            PIC 9(4) VALUE ZERO.

           01 WS-REPORTE.
               02 WS-REPORTE-SUMA PIC 9(4) VALUE ZERO.
               02 WS-REPORTE-MULTI PIC 9(5) VALUE ZERO.
               02 WS-REPORTE-MEDIA PIC 9(4) VALUE ZERO.
               02 WS-REPORTE-DOBLE.
                   04 WS-RES-DOBLE1        PIC 9(4) VALUE ZERO.
                   04 WS-RES-DOBLE2      PIC 9(4) VALUE ZERO.
                   04 WS-RES-DOBLE3       PIC 9(4) VALUE ZERO.

       procedure division.
        
       PRINCIPAL.
           display "TAREA 2"
           PERFORM INICIAR-VARIABLES.
            perform INGRESAR-DATOS.
           PERFORM SUMA.
           perform MULTIPLICACION.
           perform MEDIA.
           perform DOBLE.
           PERFORM MOSTRAR-RESULTADOS. 
       
           STOP RUN.

       INICIAR-VARIABLES.
           INITIALIZE WS-NOMBRE WS-NUMERO1 WS-NUMERO2 WS-RESULTADO 
           WS-RESULTADO-DOBLE WS-REPORTE.

       INGRESAR-DATOS.
           display "INGRESE NOMBRE".
           accept WS-NOMBRE.
           display "BIENVENIDO " WS-NOMBRE.
           display "INGRESE PRIMER NUMERO".
           accept WS-NUMERO1.
           display "INGRESE SEGUNDO NUMERO".
           accept WS-NUMERO2.
           display "INGRESE TERCER NUMERO".
           accept WS-NUMERO3.
       SUMA.
           compute WS-RESULTADO-SUMA = WS-NUMERO1 + WS-NUMERO2 
                                       + WS-NUMERO3.
      
       MULTIPLICACION.
           compute WS-RESULTADO-MULTI = (WS-NUMERO1 
           * WS-NUMERO2 * WS-NUMERO3).

       MEDIA.
           compute WS-RESULTADO-MEDIA = WS-RESULTADO-SUMA / 3.
          
       DOBLE.
           compute WS-DOBLE1 = WS-NUMERO1 * 2.
           compute WS-DOBLE2 = WS-NUMERO2 * 2.
           compute WS-DOBLE3 = WS-NUMERO3 * 2.
       
       MOSTRAR-RESULTADOS.

           MOVE WS-RESULTADO-SUMA to WS-REPORTE-SUMA.
           MOVE WS-RESULTADO-MULTI TO WS-REPORTE-MULTI.
           MOVE WS-RESULTADO-MEDIA TO WS-REPORTE-MEDIA.
           MOVE WS-DOBLE1 TO WS-RES-DOBLE1.
           MOVE WS-DOBLE2 TO WS-RES-DOBLE2.
           MOVE WS-DOBLE3 TO WS-RES-DOBLE3.
           display "--------------------------------------".
           DISPLAY "           RESULTADOS                 ".
           display "--------------------------------------".
           display "LOS NUMEROS SON: " WS-NUMERO1.
           display "" WS-NUMERO2.
           display "" WS-NUMERO3.
           DISPLAY "LA SUMA DE LOS NUMEROS ES: " WS-REPORTE-SUMA.
           display "LA MULTIPLICACION DE LOS NUMEROS ES: " 
                    WS-REPORTE-MULTI.
           display "lA MEDIA ES: "WS-REPORTE-MEDIA.
           display "EL DOBLE DE LOS NUMEROS ES:".
           display "1: " WS-RES-DOBLE1.
           display "2: " WS-RES-DOBLE2.
           display "3: " WS-RES-DOBLE3.
           display "--------------------------------------".
                  END PROGRAM PRACTICA2.