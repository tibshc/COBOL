       identification division.
       program-id. PRACTICA1.

       environment division.
       configuration section.

       data division.
       working-storage section.
        01 WS-NOMBRE        pic X(30) value zero.
        01 WS-NUMERO1       pic 9(2) value zero.
        01 WS-NUMERO2       pic 9(2) value ZERO.
        01 WS-RESULTADO     pic 9(2) value zero.
       procedure division.

       PRINCIPAL.
           display "TAREA 1"
           PERFORM INICIAR-VARIABLES.
            perform INGRESAR-DATOS.
           PERFORM SUMA.
           PERFORM MOSTRAR-RESULTADOS.

           STOP RUN.

       INICIAR-VARIABLES.
           INITIALIZE WS-NOMBRE WS-NUMERO1 WS-NUMERO2 WS-RESULTADO.

       INGRESAR-DATOS.
           display "INGRESE NOMBRE".
           accept WS-NOMBRE.
           display "BIENVENIDO " WS-NOMBRE.
           display "INGRESE PRIMER NUMERO".
           accept WS-NUMERO1.
           display "INGRESE SEGUNDO NUMERO".
           accept WS-NUMERO2.
       SUMA.
           compute WS-RESULTADO = WS-NUMERO1 + WS-NUMERO2.

       MOSTRAR-RESULTADOS.
           display "EL RESULTADO ES: " WS-RESULTADO.

       END PROGRAM PRACTICA1.