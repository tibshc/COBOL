IDENTIFICATION DIVISION.
       PROGRAM-ID. PRUEBA-SQL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * El SQLCA es indispensable para capturar errores de SQL
           EXEC SQL INCLUDE SQLCA END-EXEC.

      * Definición de variables que usaremos para conectar
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  USER-ID      PIC X(10) VALUE "admin".
       01  PASSWORD     PIC X(10) VALUE "1234".
       01  DB-NAME      PIC X(20) VALUE "mysql".
           EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Intentando conectar a la base de datos...".

      * Intentamos la conexión
      * Nota: La sintaxis puede variar levemente según la versión de OCESQL
           EXEC SQL 
               CONNECT :USER-ID IDENTIFIED BY :PASSWORD USING :DB-NAME 
           END-EXEC.

      * Verificamos si la conexión fue exitosa
           IF SQLCODE = 0
               DISPLAY "¡Conexión exitosa!"
           ELSE
               DISPLAY "Error de conexión: " SQLERRMC
           END-IF.

      * Desconectamos
           EXEC SQL DISCONNECT CURRENT END-EXEC.

           STOP RUN.