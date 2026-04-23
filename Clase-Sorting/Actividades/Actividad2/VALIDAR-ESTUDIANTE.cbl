       IDENTIFICATION DIVISION.
       PROGRAM-ID.  VALIDAR-ESTUDIANTE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MENSAJE.
           05 TEXTO-MENSAJE    PIC X(50).

       LINKAGE SECTION.
       01 REGISTRO-ESTUDIANTE.
           05 Numero-Matricula PIC 9(7).
           05 Apellido         PIC X(10).
           05 Nombre           PIC X(10).
           05 Codigo           PIC X(9).
           05 Materia          PIC X(3).
           05 Genero           PIC X(1).

       PROCEDURE DIVISION using REGISTRO-ESTUDIANTE. 
           evaluate Genero
               when "M"
                   move "Masculino" TO TEXTO-MENSAJE
               when "F"
                   move "Femenino" TO TEXTO-MENSAJE
               when other
                   move "Otro" TO TEXTO-MENSAJE
           end-evaluate.  
           DISPLAY "NOMBRE: " Nombre   " | " Apellido   " | 
           " Numero-Matricula  " | " Codigo   " | " Materia   
           " | " GENERO: " TEXTO-MENSAJE.

       END PROGRAM VALIDAR-ESTUDIANTE.