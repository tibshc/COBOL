       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRUEBA-SQL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * El SQLCA es indispensable para capturar errores de SQL
OCESQL*    EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

      * Definicion de variables que usaremos para conectar
OCESQL*    EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  USER-ID      PIC X(20) VALUE "admin".
       01  PASSWORD     PIC X(20) VALUE "1234".
      * Usamos localhost y el puerto 3306 que vimos en DBeaver
       01  DB-STRING    PIC X(50) VALUE "hola@localhost:3306".
OCESQL*    EXEC SQL END DECLARE SECTION END-EXEC.

OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(018) VALUE "DISCONNECT CURRENT".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Intentando conectar a la base de datos...".

      * Intentamos la conexion
OCESQL*    EXEC SQL
OCESQL*        CONNECT :USER-ID IDENTIFIED BY :PASSWORD USING :DB-STRING
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE USER-ID
OCESQL          BY VALUE 20
OCESQL          BY REFERENCE PASSWORD
OCESQL          BY VALUE 20
OCESQL          BY REFERENCE DB-STRING
OCESQL          BY VALUE 50
OCESQL     END-CALL.

      * Verificamos si la conexion fue exitosa
           IF SQLCODE = 0
               DISPLAY "¡Conexion exitosa!"
           ELSE
               DISPLAY "Error de conexion. SQLCODE: " SQLCODE
               DISPLAY "Mensaje: " SQLERRMC
           END-IF.

      * Desconectamos
OCESQL*    EXEC SQL DISCONNECT CURRENT END-EXEC.
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.

           STOP RUN.