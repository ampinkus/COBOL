      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL08EJ01.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WS-VAR-AUXILIARES.
              05 WS-NOMBRE            PIC X(20).
              05 WS-APELLIDO          PIC X(20).
              05 WS-DNI               PIC 99.999.999.
              05 WS-ANO-NACIMIENTO    PIC 9(04).
              05 WS-EDAD              PIC 9(03).

           77 WS-FECHA                PIC X(6).
           77 WS-FECHA-AA             PIC 99.
           77 WS-FECHA-AAAA           PIC 9999.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 0000-INICIAR-PROGRAMA
              THRU 0000-INICIAR-PROGRAMA-EXIT.

           PERFORM 2000-PROCESAR-PROGRAMA
              THRU 2000-PROCESAR-PROGRAMA-EXIT.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-EXIT.

           STOP RUN.
      *----------------------------------------------------------------*
       0000-INICIAR-PROGRAMA.

           MOVE SPACES TO WS-NOMBRE
                          WS-APELLIDO.

           MOVE ZEROES TO WS-DNI
                          WS-ANO-NACIMIENTO.

       0000-INICIAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.

           DISPLAY "Ingresa nombre de usuario:".
           ACCEPT WS-NOMBRE.

           DISPLAY "Ingresa Apellido de usuario:".
           ACCEPT WS-APELLIDO.

           DISPLAY "Ingresa DNI de usuario:".
           ACCEPT WS-DNI.

           DISPLAY "Ingresa Año de nacimiento de usuario:".
           ACCEPT WS-ANO-NACIMIENTO.

           ACCEPT WS-FECHA FROM DATE.
           MOVE WS-FECHA(1:2) TO WS-FECHA-AA.
           COMPUTE WS-FECHA-AAAA = 2000 + WS-FECHA-AA.

           COMPUTE WS-EDAD = WS-FECHA-AAAA - WS-ANO-NACIMIENTO.

       2000-PROCESAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           DISPLAY "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-".

           DISPLAY "NOMBRE   : " WS-NOMBRE.

           DISPLAY "APELLIDO : " WS-APELLIDO.

           DISPLAY "DNI      : " WS-DNI.

           DISPLAY "EDAD     : " WS-EDAD.

       3000-FINALIZAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL08EJ01.
