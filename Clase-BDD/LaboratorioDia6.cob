       IDENTIFICATION DIVISION.
       PROGRAM-ID. LaboratorioDia6.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLCA.
           05 SQLSTATE PIC X(5).
              88  SQL-SUCCESS           VALUE '00000'.
              88  SQL-RIGHT-TRUNC       VALUE '01004'.
              88  SQL-NODATA            VALUE '02000'.
              88  SQL-DUPLICATE         VALUE '23000' THRU '23999'.
              88  SQL-MULTIPLE-ROWS     VALUE '21000'.
              88  SQL-NULL-NO-IND       VALUE '22002'.
              88  SQL-INVALID-CURSOR-STATE VALUE '24000'.
           05 FILLER   PIC X.
           05 SQLVERSN PIC 99 VALUE 03.
           05 SQLCODE  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQLERRM.
               49 SQLERRML PIC S9(4) COMP-5 VALUE ZERO.
               49 SQLERRMC PIC X(486).
           05 SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5 VALUE ZERO.
           05 FILLER   PIC X(4).
           05 SQL-HCONN USAGE POINTER VALUE NULL.
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 7.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 7 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 7 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 7 TIMES.
           05 SQL-PREC   PIC X OCCURS 7 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 47.
           05 SQL-STMT   PIC X(47) VALUE 'SELECT ID,NOTA1,NOTA2,NOTA3,PR
      -    'OMEDIO FROM NOTAS'.
           05 SQL-CNAME  PIC X(7) VALUE 'CUR_ALL'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 42.
           05 SQL-STMT   PIC X(42) VALUE 'UPDATE NOTAS SET PROMEDIO = ? 
      -    'WHERE ID = ?'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 195.
           05 SQL-STMT   PIC X(195) VALUE 'SELECT ESTUDIANTES.ID,ESTUDIA
      -    'NTES.APELLIDO,ESTUDIANTES.NOMBRE,NOTAS.NOTA1,NOTAS.NOTA2,NOT
      -    'AS.NOTA3,NOTAS.PROMEDIO FROM ESTUDIANTES JOIN NOTAS ON ESTUD
      -    'IANTES.ID = NOTAS.ID WHERE ESTUDIANTES.CLASE=?'.
           05 SQL-CNAME  PIC X(9) VALUE 'CUR_CLASE'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(7) COMP-3.
           05 SQL-VAR-0002  PIC S9(3)V9(2) COMP-3.
           05 SQL-VAR-0003  PIC S9(3)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(3)V9(2) COMP-3.
           05 SQL-VAR-0005  PIC S9(3)V9(2) COMP-3.
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
      **********************************************************************
       01  WS-WORK-AREAS.
           05  WS-OP                   PIC X(1).
               88  OP-ENG              VALUE 1.
               88  OP-BUS              VALUE 2.
               88  OP-IST              VALUE 3.
               88  OP-SCI              VALUE 4.
           05  WS-SUM                  PIC 9(5)V9(2).
           05  WS-PROM                 PIC 9(2)V9(2).
           05  WS-PROM-TOTAL           PIC 9(2)V9(2).
           05  WS-PROM-ENG             PIC 9(2)V9(2).
           05  WS-PROM-BUS             PIC 9(2)V9(2).
           05  WS-PROM-IST             PIC 9(2)V9(2).
           05  WS-PROM-SCI             PIC 9(2)V9(2).

       01  DB-CONN.
           05  DB-USER                 PIC X(20) VALUE 'root'.
           05  DB-PASSWORD             PIC X(20) VALUE 'T.bobby02'.
           05  DB-NAME                 PIC X(20) VALUE 'Laboratorio'.
           05  DB-HOST                 PIC X(20) VALUE 'localhost'.
           05  DB-PORT                 PIC 9(5)  VALUE 3306.

      *    EXEC SQL
      *        BEGIN DECLARE SECTION
      *    END-EXEC
       01  DB-VARS.
           05  BUFFER                  PIC X(1024).
           05  CLASE-MENU              PIC X(3).
           05  STUDENT.
               10  IDE                 PIC 9(7).
               10  APELL               PIC X(10).
               10  NOMB                PIC X(10).
               10  CLASCOD             PIC X(9).
               10  CLAS                PIC X(3).
               10  NOTA1               PIC 9(2)V9(2).
               10  NOTA2               PIC 9(2)V9(2).
               10  NOTA3               PIC 9(2)V9(2).
               10  PROMEDIO            PIC 9(2)V9(2).
      *    EXEC SQL
      *        END DECLARE SECTION
      *    END-EXEC



       PROCEDURE DIVISION.
           PERFORM 0100-INICIO.
           PERFORM 0200-PROCESO.
           PERFORM 0300-FIN.

       0100-INICIO.

           INITIALIZE WS-WORK-AREAS.


      *-----------------------------------------------------------------
      * CONNECT TO THE DATABASE
      * also possible with DSN: 'youruser/yourpasswd@yourODBC_DSN'
      *-----------------------------------------------------------------
           STRING  'DRIVER={MySQL ODBC 8.0 ANSI Driver};'
                   'SERVER=',DB-HOST,';'
                   'PORT=',DB-PORT,';'
                   'DATABASE=',DB-NAME,';'
                   'USER=',DB-USER,';'
                   'PASSWORD=',DB-PASSWORD,';'
      * example for DB specific ODBC parameter:
      * no compressed MySQL connection (would be the DEFAULT anyway)
                   'COMRESSED_PROTO=0;'
                   INTO BUFFER.
      *    EXEC SQL
      *        CONNECT TO :BUFFER
      *    END-EXEC
           MOVE 1024 TO SQL-LEN(1)
           CALL 'OCSQL'    USING BUFFER
                               SQL-LEN(1)
                               SQLCA
           END-CALL
           PERFORM SQLSTATE-CHECK.
      *-----------------------------------------------------------------



       0200-PROCESO.

           PERFORM 0210-SELECT-NOTAS-CURSOR.
           PERFORM 0230-CALC-PROMEDIO.
           PERFORM 0240-COMMIT-SQL.
           PERFORM 0250-MENU.



       0210-SELECT-NOTAS-CURSOR.
      *-----------------------------------------------------------------
      * Lectura tabla en DB con cursor
      *-----------------------------------------------------------------
      *    EXEC SQL
      *        DECLARE CUR_ALL CURSOR FOR
      *        SELECT
      *        ID,
      *        NOTA1,
      *        NOTA2,
      *        NOTA3,
      *        PROMEDIO
      *        FROM
      *        NOTAS
      *    END-EXEC
           PERFORM SQLSTATE-CHECK.


       0220-INSERTAR-PROMEDIO.
      *-----------------------------------------------------------------
      * INSERTA PROMEDIO DEL ESTUDIANTE
      *-----------------------------------------------------------------
      *    EXEC SQL
      *        UPDATE NOTAS
      *        SET PROMEDIO = :PROMEDIO
      *        WHERE ID = :IDE
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 3 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(2)
               MOVE 4 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE PROMEDIO
             TO SQL-VAR-0005
           MOVE IDE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           PERFORM SQLSTATE-CHECK.

       0230-CALC-PROMEDIO.
      *    EXEC SQL
      *        OPEN CUR_ALL
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               MOVE 0 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-0
                               SQLCA
           END-CALL
           PERFORM SQLSTATE-CHECK.
           PERFORM UNTIL SQLCODE = 100
      *        EXEC SQL
      *            FETCH CUR_ALL
      *            INTO
      *                :IDE,
      *                :NOTA1,
      *                :NOTA2,
      *                :NOTA3,
      *                :PROMEDIO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 4 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(2)
           MOVE 3 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(3)
           MOVE 3 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(4)
           MOVE 3 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0005
           MOVE '3' TO SQL-TYPE(5)
           MOVE 3 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO IDE
           MOVE SQL-VAR-0002 TO NOTA1
           MOVE SQL-VAR-0003 TO NOTA2
           MOVE SQL-VAR-0004 TO NOTA3
           MOVE SQL-VAR-0005 TO PROMEDIO
               PERFORM SQLSTATE-CHECK
               IF SQLCODE NOT = 100
                   COMPUTE WS-SUM = NOTA1 + NOTA2 + NOTA3
                   COMPUTE WS-PROM = WS-SUM/3
                   MOVE WS-PROM TO PROMEDIO
                   PERFORM 0220-INSERTAR-PROMEDIO
               END-IF
           END-PERFORM.
      *    EXEC SQL
      *        CLOSE CUR_ALL
      *    END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA
                   .

       0240-COMMIT-SQL.
      *-----------------------------------------------------------------
      * COMMIT CHANGES
      *-----------------------------------------------------------------
      *    EXEC SQL
      *        COMMIT
      *    END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
           PERFORM SQLSTATE-CHECK.

       0250-MENU.
           DISPLAY "-----------------------------------------".
           DISPLAY "ESCOJA LA CLASE PARA VER SU RESUMEN (1-4): ".
           DISPLAY "1: ENG    2:BUS    3:IST    4:SCI".
           ACCEPT WS-OP.
           EVALUATE TRUE
               WHEN OP-ENG
                   PERFORM 0260-OP-ENG
               WHEN OP-BUS
                   PERFORM 0270-OP-BUS
               WHEN OP-IST
                   PERFORM 0280-OP-IST
               WHEN OP-SCI
                   PERFORM 0290-OP-SCI
           END-EVALUATE.

       0255-SELECT-CLASE.
      *    EXEC SQL
      *        DECLARE CUR_CLASE CURSOR FOR
      *        SELECT
      *        ESTUDIANTES.ID,
      *        ESTUDIANTES.APELLIDO,
      *        ESTUDIANTES.NOMBRE,
      *        NOTAS.NOTA1,
      *        NOTAS.NOTA2,
      *        NOTAS.NOTA3,
      *        NOTAS.PROMEDIO
      *        FROM ESTUDIANTES
      *        JOIN NOTAS ON ESTUDIANTES.ID = NOTAS.ID
      *        WHERE ESTUDIANTES.CLASE=:CLASE-MENU
      *    END-EXEC
           PERFORM SQLSTATE-CHECK.

       0260-OP-ENG.
           MOVE "ENG" TO CLASE-MENU.
           PERFORM 0255-SELECT-CLASE.
      *    EXEC SQL
      *        OPEN CUR_CLASE
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 CLASE-MENU
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 3 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-2
                               SQLCA
           END-CALL
           PERFORM SQLSTATE-CHECK.
           PERFORM UNTIL SQLCODE = 100
      *        EXEC SQL
      *            FETCH CUR_CLASE
      *            INTO
      *                :IDE,
      *                :APELL,
      *                :NOMB,
      *                :NOTA1,
      *                :NOTA2,
      *                :NOTA3,
      *                :PROMEDIO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 4 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             APELL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 10 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             NOMB
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 10 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(4)
           MOVE 3 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(5)
           MOVE 3 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(6)
           MOVE 3 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             SQL-VAR-0005
           MOVE '3' TO SQL-TYPE(7)
           MOVE 3 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
           MOVE 7 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0001 TO IDE
           MOVE SQL-VAR-0002 TO NOTA1
           MOVE SQL-VAR-0003 TO NOTA2
           MOVE SQL-VAR-0004 TO NOTA3
           MOVE SQL-VAR-0005 TO PROMEDIO
               PERFORM SQLSTATE-CHECK
               IF SQLCODE NOT = 100
                   DISPLAY IDE " " APELL " " NOMB " " NOTA1 " " NOTA2
                           " " NOTA3 " " PROMEDIO
               END-IF
           END-PERFORM.
      *    EXEC SQL
      *        CLOSE CUR_CLASE
      *    END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-2
                               SQLCA
                   .

       0270-OP-BUS.
           MOVE "BUS" TO CLASE-MENU.
           PERFORM 0255-SELECT-CLASE.
      *    EXEC SQL
      *        OPEN CUR_CLASE
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 CLASE-MENU
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 3 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-2
                               SQLCA
           END-CALL
           PERFORM SQLSTATE-CHECK.
           PERFORM UNTIL SQLCODE = 100
      *        EXEC SQL
      *            FETCH CUR_CLASE
      *            INTO
      *                :IDE,
      *                :APELL,
      *                :NOMB,
      *                :NOTA1,
      *                :NOTA2,
      *                :NOTA3,
      *                :PROMEDIO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 4 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             APELL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 10 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             NOMB
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 10 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(4)
           MOVE 3 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(5)
           MOVE 3 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(6)
           MOVE 3 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             SQL-VAR-0005
           MOVE '3' TO SQL-TYPE(7)
           MOVE 3 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
           MOVE 7 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0001 TO IDE
           MOVE SQL-VAR-0002 TO NOTA1
           MOVE SQL-VAR-0003 TO NOTA2
           MOVE SQL-VAR-0004 TO NOTA3
           MOVE SQL-VAR-0005 TO PROMEDIO
               PERFORM SQLSTATE-CHECK
               IF SQLCODE NOT = 100
                   DISPLAY IDE " " APELL " " NOMB " " NOTA1 " " NOTA2
                           " " NOTA3 " " PROMEDIO
               END-IF
           END-PERFORM.
      *    EXEC SQL
      *        CLOSE CUR_CLASE
      *    END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-2
                               SQLCA
                   .

       0280-OP-IST.
           MOVE "IST" TO CLASE-MENU.
           PERFORM 0255-SELECT-CLASE.
      *    EXEC SQL
      *        OPEN CUR_CLASE
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 CLASE-MENU
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 3 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-2
                               SQLCA
           END-CALL
           PERFORM SQLSTATE-CHECK.
           PERFORM UNTIL SQLCODE = 100
      *        EXEC SQL
      *            FETCH CUR_CLASE
      *            INTO
      *                :IDE,
      *                :APELL,
      *                :NOMB,
      *                :NOTA1,
      *                :NOTA2,
      *                :NOTA3,
      *                :PROMEDIO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 4 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             APELL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 10 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             NOMB
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 10 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(4)
           MOVE 3 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(5)
           MOVE 3 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(6)
           MOVE 3 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             SQL-VAR-0005
           MOVE '3' TO SQL-TYPE(7)
           MOVE 3 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
           MOVE 7 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0001 TO IDE
           MOVE SQL-VAR-0002 TO NOTA1
           MOVE SQL-VAR-0003 TO NOTA2
           MOVE SQL-VAR-0004 TO NOTA3
           MOVE SQL-VAR-0005 TO PROMEDIO
               PERFORM SQLSTATE-CHECK
               IF SQLCODE NOT = 100
                   DISPLAY IDE " " APELL " " NOMB " " NOTA1 " " NOTA2
                           " " NOTA3 " " PROMEDIO
               END-IF
           END-PERFORM.
      *    EXEC SQL
      *        CLOSE CUR_CLASE
      *    END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-2
                               SQLCA
                   .

       0290-OP-SCI.
           MOVE "SCI" TO CLASE-MENU.
           PERFORM 0255-SELECT-CLASE.
      *    EXEC SQL
      *        OPEN CUR_CLASE
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 CLASE-MENU
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 3 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-2
                               SQLCA
           END-CALL
           PERFORM SQLSTATE-CHECK.
           PERFORM UNTIL SQLCODE = 100
      *        EXEC SQL
      *            FETCH CUR_CLASE
      *            INTO
      *                :IDE,
      *                :APELL,
      *                :NOMB,
      *                :NOTA1,
      *                :NOTA2,
      *                :NOTA3,
      *                :PROMEDIO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 4 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             APELL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 10 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             NOMB
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 10 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(4)
           MOVE 3 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(5)
           MOVE 3 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(6)
           MOVE 3 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             SQL-VAR-0005
           MOVE '3' TO SQL-TYPE(7)
           MOVE 3 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
           MOVE 7 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0001 TO IDE
           MOVE SQL-VAR-0002 TO NOTA1
           MOVE SQL-VAR-0003 TO NOTA2
           MOVE SQL-VAR-0004 TO NOTA3
           MOVE SQL-VAR-0005 TO PROMEDIO
               PERFORM SQLSTATE-CHECK
               IF SQLCODE NOT = 100
                   DISPLAY IDE " " APELL " " NOMB " " NOTA1 " " NOTA2
                           " " NOTA3 " " PROMEDIO
               END-IF
           END-PERFORM.
      *    EXEC SQL
      *        CLOSE CUR_CLASE
      *    END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-2
                               SQLCA
                   .

       0300-FIN.

      *-----------------------------------------------------------------
      * DISCONNECT FROM THE DATABASE
      *-----------------------------------------------------------------
      *    EXEC SQL
      *        CONNECT RESET
      *    END-EXEC
           CALL 'OCSQLDIS' USING SQLCA END-CALL
           PERFORM SQLSTATE-CHECK.
      *-----------------------------------------------------------------

           STOP RUN.

      *-----------------------------------------------------------------
      * REVISA SQLSTATE E IMPRIME ERRORES SI EXISTEN
      *-----------------------------------------------------------------
       SQLSTATE-CHECK.
           IF SQLCODE < 0
               DISPLAY 'SQLSTATE='  SQLSTATE,
                       ', SQLCODE=' SQLCODE
               IF SQLERRML > 0
                   DISPLAY 'SQL Error message:' SQLERRMC(1:SQLERRML)
               END-IF
               MOVE SQLCODE TO RETURN-CODE
               STOP RUN
           ELSE
               IF SQLCODE > 0 AND NOT = 100
                   DISPLAY 'SQLSTATE='  SQLSTATE,
                           ', SQLCODE=' SQLCODE
                   IF SQLERRML > 0
                       DISPLAY 'SQL Warning message:'
                               SQLERRMC(1:SQLERRML)
                   END-IF
               END-IF.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  APELL                    IN USE CHAR(10)
      *  BUFFER                   IN USE CHAR(1024)
      *  CLAS                 NOT IN USE
      *  CLASCOD              NOT IN USE
      *  CLASE-MENU               IN USE CHAR(3)
      *  CUR_ALL                  IN USE CURSOR
      *  CUR_CLASE                IN USE CURSOR
      *  DB-VARS              NOT IN USE
      *  DB-VARS.APELL        NOT IN USE
      *  DB-VARS.BUFFER       NOT IN USE
      *  DB-VARS.CLAS         NOT IN USE
      *  DB-VARS.CLASCOD      NOT IN USE
      *  DB-VARS.CLASE-MENU   NOT IN USE
      *  DB-VARS.IDE          NOT IN USE
      *  DB-VARS.NOMB         NOT IN USE
      *  DB-VARS.NOTA1        NOT IN USE
      *  DB-VARS.NOTA2        NOT IN USE
      *  DB-VARS.NOTA3        NOT IN USE
      *  DB-VARS.PROMEDIO     NOT IN USE
      *  DB-VARS.STUDENT      NOT IN USE
      *  IDE                      IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(7,0)
      *  NOMB                     IN USE CHAR(10)
      *  NOTA1                    IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(5,2)
      *  NOTA2                    IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(5,2)
      *  NOTA3                    IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(5,2)
      *  PROMEDIO                 IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(5,2)
      *  STUDENT              NOT IN USE
      **********************************************************************
